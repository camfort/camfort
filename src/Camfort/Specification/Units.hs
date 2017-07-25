{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-

  Units of measure extension to Fortran

-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Camfort.Specification.Units
  ( checkUnits
  , inferUnits
  , synthesiseUnits
  , inferCriticalVariables
  , chooseImplicitNames
  ) where

import qualified Data.Map.Strict as M
import Data.Data
import Data.List (intercalate, find, sort, group, nub, inits)
import Data.Maybe (fromMaybe, maybeToList, mapMaybe, maybe)
import Data.Generics.Uniplate.Operations
import qualified Data.ByteString.Char8 as B
import GHC.Generics (Generic)

import Camfort.Helpers
import Camfort.Analysis.Annotations
import Camfort.Analysis.Fortran
  (Analysis, analysisInput, analysisResult, branchAnalysis, writeDebug)
import Camfort.Input

-- Provides the types and data accessors used in this module
import           Camfort.Specification.Units.Analysis (UnitsAnalysis)
import qualified Camfort.Specification.Units.Annotation as UA
import           Camfort.Specification.Units.Environment
import           Camfort.Specification.Units.InferenceFrontend
import           Camfort.Specification.Units.ModFile (initializeModFiles)
import           Camfort.Specification.Units.Monad
import           Camfort.Specification.Units.Synthesis (runSynthesis)

import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Util.Position as FU
import Language.Fortran.Util.ModFile

-- | Run a unit inference.
runInference :: UnitOpts
             -> F.ProgramFile UA
             -> UnitSolver a
             -> (Either UnitException a, UnitState, Report)
runInference uOpts pf runner =
  let (r, s, report) = runUnitSolver uOpts pf $ do
        initializeModFiles $ uoModFiles uOpts
        initInference
        runner
  in (r, s, mkReport report)

-- *************************************
--   Unit inference (top - level)
--
-- *************************************

{-| Infer one possible set of critical variables for a program -}
inferCriticalVariables
  :: UnitOpts
  -> UnitsAnalysis (F.ProgramFile Annotation) Int
inferCriticalVariables uOpts = do
  pf <- analysisInput
  let
    fname = F.pfGetFilename pf
    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (uoModFiles uOpts)
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap UA.mkUnitAnnotation $ pf
    mmap' = extractModuleMap pfRenamed `M.union` mmap -- post-parsing
    -- unique name -> src name across modules

    -- Map of all declarations
    dmap = extractDeclMap pfRenamed `M.union` combinedDeclMap (uoModFiles uOpts)
    uniqnameMap = M.fromList [
                (FA.varName e, FA.srcName e) |
                e@(F.ExpValue _ _ F.ValVariable{}) <- universeBi pfRenamed :: [F.Expression UA]
                -- going to ignore intrinsics here
              ] `M.union` (M.unions . map (M.fromList . map (\ (a, (b, _)) -> (b, a)) . M.toList) $ M.elems mmap')
    (eVars, state, logs) = runInference uOpts pfRenamed runCriticalVariables
  writeDebug logs
  case eVars of
    Right vars -> okReport fname dmap vars uniqnameMap
    Left exc   -> errReport fname exc
  where
    -- Format report
    okReport fname _ [] _ = do
      writeDebug . mkReport $ concat ["\n", fname, ":No additional annotations are necessary.\n"]
      pure 0
    okReport fname dmap vars uniqnameMap = do
      writeDebug . mkReport $ concat ["\n", fname, ": ", show numVars
                                     , " variable declarations suggested to be given a specification:\n"
                                     , unlines [ "    " ++ declReport d | d <- M.toList dmapSlice ]]
      pure numVars
      where
        varNames  = map unitVarName vars
        dmapSlice = M.filterWithKey (\ k _ -> k `elem` varNames) dmap
        numVars   = M.size dmapSlice
        declReport (v, (dc, ss)) = vfilename ++ " (" ++ showSrcSpan ss ++ ")    " ++ fromMaybe v (M.lookup v uniqnameMap)
          where vfilename = fromMaybe fname $ M.lookup v fromWhereMap

    unitVarName (UnitVar (v, _))                 = v
    unitVarName (UnitParamVarUse (_, (v, _), _)) = v
    unitVarName _                                = "<bad>"

    errReport fname exc = do
      writeDebug . mkReport $ concat ["\n", fname, ":\n", show exc]
      pure (-1)

    fromWhereMap = genUniqNameToFilenameMap $ uoModFiles uOpts

data ConsistencyReport
    -- | All units were consistent.
  = forall a. Consistent (F.ProgramFile a) Int
    -- | An inconsistency was found in units of the program.
  | Inconsistent ConsistencyError

instance Show ConsistencyReport where
  show (Consistent pf nVars) = concat ["\n", fname, ": Consistent ", show nVars, " variables checked."]
    where fname = F.pfGetFilename pf
  show (Inconsistent e) = show e

data ConsistencyError =
  Inconsistency (F.ProgramFile UA) UnitState Constraints

instance Show ConsistencyError where
  show (Inconsistency pf state cons) = concat [ "\n", fname, ": Inconsistent:\n", reportErrors cons, "\n\n"
                                       , unlines (map showSrcConstraint constraints)]
    where
      fname = F.pfGetFilename pf
      showSrcConstraint :: (Constraint, FU.SrcSpan) -> String
      showSrcConstraint (con, srcSpan) = show srcSpan ++ ": " ++ show con
      reportErrors cons = unlines [ maybe "" showSS ss ++ str | (ss, str) <- reports ]
        where
          reports = map head . group . sort . map reportError . filter relevantConstraints $ cons
          showSS  = (++ ": ") . (" - at "++) . showSrcSpan

          relevantConstraints c = not (isPolymorphic0 c) && not (isReflexive c)

          isPolymorphic0 (ConEq UnitParamLitAbs{} _) = True
          isPolymorphic0 (ConEq _ UnitParamLitAbs{}) = True
          isPolymorphic0 _                         = False

          isReflexive (ConEq u1 u2) = u1 == u2

      reportError con = (span, pprintConstr (orient (unrename con)) ++ additionalInfo con)
        where
          span = findCon con
          -- Create additional info for errors
          additionalInfo con =
             if null (errorInfo con)
             then ""
             else "\n    instead" ++ intercalate "\n" (mapNotFirst (pad 10) (errorInfo con))
          -- Create additional info about inconsistencies involving variables
          errorInfo con =
              [" '" ++ sName ++ "' is '" ++ pprintUnitInfo (unrename u) ++ "'"
                | UnitVar (vName, sName) <- universeBi con
                , u                       <- findUnitConstrFor con vName ]
          -- Find unit information for variable constraints
          findUnitConstrFor con v = mapMaybe (\con' -> if con == con'
                                                       then Nothing
                                                       else constrainedTo v con')
                                             (concat $ M.elems templateMap)
          constrainedTo v (ConEq (UnitVar (v', _)) u) | v == v' = Just u
          constrainedTo v (ConEq u (UnitVar (v', _))) | v == v' = Just u
          constrainedTo _ _ = Nothing

          mapNotFirst f [] = []
          mapNotFirst f (x : xs) =  x : fmap f xs

          orient (ConEq u (UnitVar v)) = ConEq (UnitVar v) u
          orient (ConEq u (UnitParamVarUse v)) = ConEq (UnitParamVarUse v) u
          orient c = c

          pad o = (++) (replicate o ' ')

          srcSpan con | Just ss <- findCon con = showSrcSpan ss ++ " "
                      | otherwise                   = ""

      findCon :: Constraint -> Maybe FU.SrcSpan
      findCon con = lookupWith (eq con) constraints
        where eq c1 c2 = or [ conParamEq c1 c2' | c2' <- universeBi c2 ]
      templateMap = usTemplateMap state
      constraints = [ (c, srcSpan)
                    | x <- universeBi pf :: [F.Expression UA]
                    , let srcSpan = FU.getSpan x
                    , c <- maybeToList (UA.getConstraint x)
                    ] ++

                    [ (c, srcSpan)
                    | x <- universeBi pf :: [F.Statement UA]
                    , let srcSpan = FU.getSpan x
                    , c <- maybeToList (UA.getConstraint x)
                    ] ++

                    [ (c, srcSpan)
                    | x <- universeBi pf :: [F.Argument UA]
                    , let srcSpan = FU.getSpan x
                    , c <- maybeToList (UA.getConstraint x)
                    ] ++

                    [ (c, srcSpan)
                    | x <- universeBi pf :: [F.Declarator UA]
                    , let srcSpan = FU.getSpan x
                    , c <- maybeToList (UA.getConstraint x)
                    ] ++

                    -- Why reverse? So that PUFunction and PUSubroutine appear
                    -- first in the list, before PUModule.
                    reverse [ (c, srcSpan)
                    | x <- universeBi pf :: [F.ProgramUnit UA]
                    , let srcSpan = FU.getSpan x
                    , c <- maybeToList (UA.getConstraint x)
                    ]

{-| Check units-of-measure for a program -}
checkUnits :: UnitOpts -> UnitsAnalysis (F.ProgramFile Annotation) ConsistencyReport
checkUnits uOpts = do
  pf <- analysisInput
  let
    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (uoModFiles uOpts)
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap UA.mkUnitAnnotation $ pf
    (eCons, state, logs) = runInference uOpts pfRenamed runInconsistentConstraints
    -- number of 'real' variables checked, e.g. not parametric
    nVars = M.size . M.filter (not . isParametricUnit) $ usVarUnitMap state
    pfUA :: F.ProgramFile UA
    pfUA = usProgramFile state -- the program file after units analysis is done
  writeDebug logs
  pure $ case eCons of
           Right Nothing -> Consistent pf nVars
           Right (Just cons) -> Inconsistent $ Inconsistency pfUA state cons
           -- FIXME: What does this mean... It's not tested...?
           Left e -> undefined
  where
    isParametricUnit u = case u of UnitParamPosAbs {} -> True; UnitParamPosUse {} -> True
                                   UnitParamVarAbs {} -> True; UnitParamVarUse {} -> True
                                   _ -> False

lookupWith :: (a -> Bool) -> [(a,b)] -> Maybe b
lookupWith f = fmap snd . find (f . fst)


-- | Create unique names for all of the inferred implicit polymorphic
-- unit variables.
chooseImplicitNames :: [(VV, UnitInfo)] -> [(VV, UnitInfo)]
chooseImplicitNames vars = replaceImplicitNames (genImplicitNamesMap vars) vars

genImplicitNamesMap :: Data a => a -> M.Map UnitInfo UnitInfo
genImplicitNamesMap x = M.fromList [ (absU, UnitParamEAPAbs (newN, newN)) | (absU, newN) <- zip absUnits newNames ]
  where
    absUnits = nub [ u | u@(UnitParamPosAbs _)             <- universeBi x ]
    eapNames = nub $ [ n | u@(UnitParamEAPAbs (_, n))      <- universeBi x ] ++
                     [ n | u@(UnitParamEAPUse ((_, n), _)) <- universeBi x ]
    newNames = filter (`notElem` eapNames) . map ('\'':) $ nameGen
    nameGen  = concatMap sequence . tail . inits $ repeat ['a'..'z']

replaceImplicitNames :: Data a => M.Map UnitInfo UnitInfo -> a -> a
replaceImplicitNames implicitMap = transformBi replace
  where
    replace u@(UnitParamPosAbs _) = fromMaybe u $ M.lookup u implicitMap
    replace u                     = u

-- | Report from unit inference.
data InferenceReport =
  Inferred (F.ProgramFile UA) [(VV, UnitInfo)]

instance Show InferenceReport where
  show (Inferred pf vars) =
    concat ["\n", fname, ":\n", unlines [ expReport ei | ei <- expInfo ]]
    where
      expReport (ExpInfo ss vname sname, u) = "  " ++ showSrcSpan ss ++ " unit " ++ show u ++ " :: " ++ sname
      fname = F.pfGetFilename pf
      expInfo = [ (ei, u) | ei@(ExpInfo _ vname sname) <- declVariableNames pf
                          , u <- maybeToList ((vname, sname) `lookup` vars) ]
      -- | List of declared variables (including both decl statements & function returns, defaulting to first)
      declVariableNames :: F.ProgramFile UA -> [ExpInfo]
      declVariableNames pf = sort . M.elems $ M.unionWith (curry fst) declInfo puInfo
        where
          declInfo = M.fromList [ (expInfoVName ei, ei) | ei <- declVariableNamesDecl pf ]
          puInfo   = M.fromList [ (expInfoVName ei, ei) | ei <- declVariableNamesPU pf ]
      declVariableNamesDecl :: F.ProgramFile UA -> [ExpInfo]
      declVariableNamesDecl pf = flip mapMaybe (universeBi pf :: [F.Declarator UA]) $ \ d -> case d of
        F.DeclVariable _ ss v@(F.ExpValue _ _ (F.ValVariable _)) _ _   -> Just (ExpInfo ss (FA.varName v) (FA.srcName v))
        F.DeclArray    _ ss v@(F.ExpValue _ _ (F.ValVariable _)) _ _ _ -> Just (ExpInfo ss (FA.varName v) (FA.srcName v))
        _                                                             -> Nothing
      declVariableNamesPU :: F.ProgramFile UA -> [ExpInfo]
      declVariableNamesPU pf = flip mapMaybe (universeBi pf :: [F.ProgramUnit UA]) $ \ pu -> case pu of
        F.PUFunction _ ss _ _ _ _ (Just v@(F.ExpValue _ _ (F.ValVariable _))) _ _ -> Just (ExpInfo ss (FA.varName v) (FA.srcName v))
        F.PUFunction _ ss _ _ _ _ Nothing _ _                                     -> Just (ExpInfo ss (puName pu) (puSrcName pu))
        _                                                                         -> Nothing


getInferred :: InferenceReport -> [(VV, UnitInfo)]
getInferred (Inferred _ vars) = vars

{-| Check and infer units-of-measure for a program
    This produces an output of all the unit information for a program -}
inferUnits :: UnitOpts -> UnitsAnalysis (F.ProgramFile Annotation) (Either ConsistencyError InferenceReport)
inferUnits uOpts = do
  pf <- analysisInput
  let
      -- Use the module map derived from all of the included Camfort Mod files.
      mmap = combinedModuleMap (uoModFiles uOpts)
      pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap UA.mkUnitAnnotation $ pf
      (eVars, state, logs) = runInference uOpts pfRenamed (chooseImplicitNames <$> runInferVariables)
      pfUA = usProgramFile state -- the program file after units analysis is done
  consistency <- checkUnits uOpts
  writeDebug logs
  pure $ case consistency of
           Consistent{}     ->
             case eVars of
               -- FIXME: What does this mean... It's not tested...?
               Left e -> undefined
               Right vars   -> Right $ Inferred pfUA vars
           Inconsistent err -> Left err

synthesiseUnits :: UnitOpts
                -> Char
                -> UnitsAnalysis
                   (F.ProgramFile Annotation)
                   (Either ConsistencyError (InferenceReport, F.ProgramFile Annotation))
{-| Synthesis unspecified units for a program (after checking) -}
synthesiseUnits uOpts marker = do
  infRes <- inferUnits uOpts
  case infRes of
    Left err       -> pure $ Left err
    Right inferred -> do
      pf <- analysisInput
      pure . Right $ (inferred, runSynth pf (getInferred inferred))
  where
    runSynth pf inferred =
      let (eVars, state, logs) = runInference uOpts pfRenamed (runSynthesis marker . chooseImplicitNames $ inferred)
          -- Use the module map derived from all of the included Camfort Mod files.
          mmap = combinedModuleMap (uoModFiles uOpts)
          pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap UA.mkUnitAnnotation $ pf
          pfUA = usProgramFile state -- the program file after units analysis is done
      in fmap (UA.prevAnnotation . FA.prevAnnotation) pfUA -- strip annotations

--------------------------------------------------

-- clear out the unique names in the UnitInfos.
unrename :: Data a => a -> a
unrename = transformBi $ \ x -> case x of
  UnitVar (u, s)                      -> UnitVar (s, s)
  UnitParamVarAbs ((_, f), (_, s))    -> UnitParamVarAbs ((f, f), (s, s))
  UnitParamVarUse ((_, f), (_, s), i) -> UnitParamVarUse ((f, f), (s, s), i)
  UnitParamEAPAbs (_, s)              -> UnitParamEAPAbs (s, s)
  UnitParamEAPUse ((_, s), i)         -> UnitParamEAPUse ((s, s), i)
  u                                   -> u


showSrcSpan :: FU.SrcSpan -> String
showSrcSpan (FU.SrcSpan l u) = show l

data ExpInfo = ExpInfo { expInfoSrcSpan :: FU.SrcSpan, expInfoVName :: F.Name, expInfoSName :: F.Name }
  deriving (Show, Eq, Ord, Typeable, Data, Generic)
