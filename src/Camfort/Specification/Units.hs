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
  , compileUnits
  , synthesiseUnits
  , inferCriticalVariables
  , chooseImplicitNames
  ) where

import qualified Data.Map.Strict as M
import Data.Data
import Data.List (intercalate, find, sort, group, nub, inits)
import Data.Maybe (fromMaybe, maybeToList, mapMaybe, maybe)
import Data.Binary
import Data.Generics.Uniplate.Operations
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Generics (Generic)

import Camfort.Analysis
  (Analysis, Refactoring, mkAnalysis, runAnalysis)
import Camfort.Helpers
import Camfort.Analysis.Annotations
import Camfort.Input
import Camfort.Reprint (subtext)

-- Provides the types and data accessors used in this module
import qualified Camfort.Specification.Units.Annotation as UA
import           Camfort.Specification.Units.Environment
import           Camfort.Specification.Units.InferenceFrontend
import           Camfort.Specification.Units.Monad
import           Camfort.Specification.Units.Synthesis (runSynthesis)

import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Util.Position as FU
import Language.Fortran.Util.ModFile

-- | Analysis with additional debugging information via a 'Report'.
type UnitsAnalysis r a = Analysis (Report, r) a
type UnitsRefactoring r a a' = Refactoring (Report, Maybe r) a a'

-- | Run a unit inference.
runInference :: UnitOpts
             -> F.ProgramFile UA
             -> UnitSolver a
             -> (Either UnitException a, UnitState, UnitLogs)
runInference uOpts pf runner = runUnitSolver uOpts pf $ do
  let compiledUnits = combinedCompiledUnits . M.elems . uoModFiles $ uOpts
  modifyTemplateMap  . const . cuTemplateMap  $ compiledUnits
  modifyNameParamMap . const . cuNameParamMap $ compiledUnits
  initInference
  runner

-- *************************************
--   Unit inference (top - level)
--
-- *************************************

{-| Infer one possible set of critical variables for a program -}
inferCriticalVariables
  :: UnitOpts
  -> Analysis (Report, Int) (F.ProgramFile Annotation)
inferCriticalVariables uOpts = mkAnalysis (inferCriticalVariables' uOpts)

inferCriticalVariables' :: UnitOpts -> F.ProgramFile Annotation -> (Report, Int)
inferCriticalVariables' uOpts pf
  | Right vars <- eVars = okReport vars
  | Left exc   <- eVars = (errReport exc, -1)
  where
    fname = F.pfGetFilename pf
    -- Format report
    okReport []   = (logs ++ "\n" ++ fname
                         ++ ":No additional annotations are necessary.\n", 0)
    okReport vars = ( logs ++ "\n" ++ fname ++ ": "
                           ++ show numVars
                           ++ " variable declarations suggested to be given a specification:\n"
                           ++ unlines [ "    " ++ declReport d | d <- M.toList dmapSlice ]
                    , numVars)
      where
        varNames  = map unitVarName vars
        dmapSlice = M.filterWithKey (\ k _ -> k `elem` varNames) dmap
        numVars   = M.size dmapSlice

    declReport (v, (dc, ss)) = vfilename ++ " (" ++ showSrcSpan ss ++ ")    " ++ fromMaybe v (M.lookup v uniqnameMap)
      where vfilename = fromMaybe fname $ M.lookup v fromWhereMap

    unitVarName (UnitVar (v, _))                 = v
    unitVarName (UnitParamVarUse (_, (v, _), _)) = v
    unitVarName _                                = "<bad>"

    errReport exc = logs ++ "\n" ++ fname ++ ":\n" ++ show exc

    (eVars, state, logs) = runInference uOpts pfRenamed runCriticalVariables
    pfUA = usProgramFile state -- the program file after units analysis is done

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uOpts))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap UA.mkUnitAnnotation $ pf

    -- Map of all declarations
    dmap = extractDeclMap pfRenamed `M.union` combinedDeclMap (M.elems (uoModFiles uOpts))

    mmap' = extractModuleMap pfRenamed `M.union` mmap -- post-parsing
    -- unique name -> src name across modules
    uniqnameMap = M.fromList [
                (FA.varName e, FA.srcName e) |
                e@(F.ExpValue _ _ F.ValVariable{}) <- universeBi pfRenamed :: [F.Expression UA]
                -- going to ignore intrinsics here
              ] `M.union` (M.unions . map (M.fromList . map (\ (a, (b, _)) -> (b, a)) . M.toList) $ M.elems mmap')
    fromWhereMap = genUniqNameToFilenameMap . M.elems $ uoModFiles uOpts

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
checkUnits :: UnitOpts -> UnitsAnalysis ConsistencyReport (F.ProgramFile Annotation)
checkUnits uOpts = mkAnalysis (checkUnits' uOpts)

checkUnits' :: UnitOpts -> F.ProgramFile Annotation -> (Report, ConsistencyReport)
checkUnits' uOpts pf =
  (logs, case eCons of
    Right Nothing -> Consistent pf nVars
    Right (Just cons) -> Inconsistent $ Inconsistency pfUA state cons
    -- FIXME: What does this mean... It's not tested...?
    Left e -> undefined)
  where

    -- Find a given constraint within the annotated AST. FIXME: optimise

    varReport     = intercalate ", " . map showVar

    showVar (UnitVar (_, s)) = s
    showVar (UnitLiteral _)   = "<literal>" -- FIXME
    showVar _                 = "<bad>"

    (eCons, state, logs) = runInference uOpts pfRenamed runInconsistentConstraints
    pfUA :: F.ProgramFile UA
    pfUA = usProgramFile state -- the program file after units analysis is done

    -- number of 'real' variables checked, e.g. not parametric
    nVars = M.size . M.filter (not . isParametricUnit) $ usVarUnitMap state
    isParametricUnit u = case u of UnitParamPosAbs {} -> True; UnitParamPosUse {} -> True
                                   UnitParamVarAbs {} -> True; UnitParamVarUse {} -> True
                                   _ -> False

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uOpts))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap UA.mkUnitAnnotation $ pf

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
inferUnits :: UnitOpts -> UnitsAnalysis (Either ConsistencyError InferenceReport) (F.ProgramFile Annotation)
inferUnits uOpts = mkAnalysis (inferUnits' uOpts)

inferUnits' :: UnitOpts -> F.ProgramFile Annotation -> (Report, Either ConsistencyError InferenceReport)
inferUnits' uOpts pf =
  case runAnalysis (checkUnits uOpts) pf of
    (debug, Consistent{})     ->
      case eVars of
        -- FIXME: What does this mean... It's not tested...?
        Left e -> undefined
        Right vars -> (debug ++ logs, pure $ Inferred pfUA vars)
        -- (debug ++ logs, pure $ Inferred pf eVars)
    (debug, Inconsistent err) -> (debug ++ logs, Left err)
  where
    fname = F.pfGetFilename pf

    errReport exc = logs ++ "\n" ++ fname ++ ":  " ++ show exc

    (eVars, state, logs) = runInference uOpts pfRenamed (chooseImplicitNames <$> runInferVariables)

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uOpts))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap UA.mkUnitAnnotation $ pf
    pfUA = usProgramFile state -- the program file after units analysis is done

combinedCompiledUnits :: ModFiles -> CompiledUnits
combinedCompiledUnits mfs = CompiledUnits { cuTemplateMap = M.unions tmaps
                                          , cuNameParamMap = M.unions nmaps }
  where
    cus = map mfCompiledUnits mfs
    tmaps = map cuTemplateMap cus
    nmaps = map cuNameParamMap cus

-- | Name of the labeled data within a ModFile containing unit-specific info.
unitsCompiledDataLabel = "units-compiled-data"

mfCompiledUnits :: ModFile -> CompiledUnits
mfCompiledUnits mf = case lookupModFileData unitsCompiledDataLabel mf of
  Nothing -> emptyCompiledUnits
  Just bs -> case decodeOrFail (LB.fromStrict bs) of
    Left _ -> emptyCompiledUnits
    Right (_, _, cu) -> cu

genUnitsModFile :: F.ProgramFile UA -> CompiledUnits -> ModFile
genUnitsModFile pf cu = alterModFileData f unitsCompiledDataLabel $ genModFile pf
  where
    f _ = Just . LB.toStrict $ encode cu

compileUnits :: UnitOpts -> Refactoring String [FileProgram] [(Filename, B.ByteString)]
compileUnits uOpts fileprogs = (concat reports, concat bins)
  where
    (reports, bins) = unzip [ (report, bin) | fileprog <- fileprogs
                                            , let (report, bin) = compileUnits' uOpts fileprog ]

compileUnits' :: UnitOpts -> Refactoring String FileProgram [(Filename, B.ByteString)]
compileUnits' uOpts pf
  | Right cu <- eCUnits = okReport cu
  | Left exc <- eCUnits = errReport exc
  where
    fname = F.pfGetFilename pf
    -- Format report
    okReport cu = ( logs ++ "\n" ++ fname ++ ":\n" ++ if uoDebug uOpts then debugInfo else []
                     -- FIXME, filename manipulation (needs to go in -I dir?)
                    , [(fname ++ modFileSuffix, encodeModFile (genUnitsModFile pfTyped cu))] )
      where
        debugInfo = unlines [ n ++ ":\n  " ++ intercalate "\n  " (map show cs) | (n, cs) <- M.toList (cuTemplateMap cu) ] ++
                    unlines ("nameParams:" : (map show . M.toList $ cuNameParamMap cu))


    errReport exc = ( logs ++ "\n" ++ fname ++ ":  " ++ show exc
                    , [] )
    (eCUnits, state, logs) = runInference uOpts pfTyped runCompileUnits
    pfUA = usProgramFile state -- the program file after units analysis is done

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uOpts))
    tenv = combinedTypeEnv (M.elems (uoModFiles uOpts))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap UA.mkUnitAnnotation $ pf
    pfTyped = fst . FAT.analyseTypesWithEnv tenv $ pfRenamed

synthesiseUnits :: UnitOpts
                -> Char
                -> UnitsRefactoring InferenceReport
                   (F.ProgramFile Annotation)
                   (Either ConsistencyError (F.ProgramFile Annotation))
{-| Synthesis unspecified units for a program (after checking) -}
synthesiseUnits uOpts marker pf =
  case runAnalysis (inferUnits uOpts) pf of
    (debug, Left err)       -> ((debug, Nothing), Left err)
    (debug, Right inferred) -> ((debug, Just inferred), pure $ runSynth (getInferred inferred))
  where
    fname = F.pfGetFilename pf
    runSynth inferred =
      let (eVars, state, logs) = runInference uOpts pfRenamed (runSynthesis marker . chooseImplicitNames $ inferred)
          -- Use the module map derived from all of the included Camfort Mod files.
          mmap = combinedModuleMap (M.elems (uoModFiles uOpts))
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
