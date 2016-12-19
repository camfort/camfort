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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Camfort.Specification.Units
  (checkUnits, inferUnits, compileUnits, synthesiseUnits, inferCriticalVariables)
where

import qualified Data.Map.Strict as M
import Data.Char (isNumber)
import Data.List (intercalate, find, sort, group)
import Data.Maybe (fromMaybe, maybeToList, listToMaybe, mapMaybe, isJust, maybe)
import Data.Binary
import Data.Generics.Uniplate.Operations
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Monad.State.Strict

import Camfort.Helpers hiding (lineCol)
import Camfort.Helpers.Syntax
import Camfort.Output
import Camfort.Analysis.Annotations
import Camfort.Input

-- Provides the types and data accessors used in this module
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.Monad
import Camfort.Specification.Units.InferenceBackend
import Camfort.Specification.Units.InferenceFrontend
import Camfort.Specification.Units.Synthesis (runSynthesis)

import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Util.Position as FU
import Language.Fortran.Util.ModFile

-- For debugging and development purposes
import qualified Debug.Trace as D

-- *************************************
--   Unit inference (top - level)
--
-- *************************************

inferCriticalVariables
  :: UnitOpts -> (Filename, F.ProgramFile Annotation) -> (Report, Int)

{-| Infer one possible set of critical variables for a program -}
inferCriticalVariables uo (fname, pf)
  | Right vars <- eVars = okReport vars
  | Left exc   <- eVars = (errReport exc, -1)
  where
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

    declReport (v, (dc, ss)) = "(" ++ showSrcSpan ss ++ ")    " ++ fromMaybe v (M.lookup v uniqnameMap)

    unitVarName (UnitVar (v, _))                 = v
    unitVarName (UnitParamVarUse (_, (v, _), _)) = v
    unitVarName _                                = "<bad>"

    errReport exc = logs ++ "\n" ++ fname ++ ":\n" ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = FAR.extractNameMap pfRenamed }
    (eVars, state, logs) = runUnitSolver uOpts pfRenamed $ do
      modifyTemplateMap . const . combinedTemplateMap . M.elems $ uoModFiles uo
      initInference
      runCriticalVariables
    pfUA = usProgramFile state -- the program file after units analysis is done

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uo))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap mkUnitAnnotation $ pf

    -- Map of all declarations
    dmap = extractDeclMap pfRenamed `M.union` combinedDeclMap (M.elems (uoModFiles uo))

    mmap' = extractModuleMap pfRenamed `M.union` mmap -- post-parsing
    -- unique name -> src name across modules
    uniqnameMap = M.fromList [
                (FA.varName e, FA.srcName e) |
                e@(F.ExpValue _ _ (F.ValVariable {})) <- universeBi pfRenamed :: [F.Expression UA]
              ] `M.union` (M.unions . map (M.fromList . map (\ (a, (b, _)) -> (b, a)) . M.toList) $ M.elems mmap')

checkUnits, inferUnits
            :: UnitOpts -> (Filename, F.ProgramFile Annotation) -> Report
{-| Check units-of-measure for a program -}
checkUnits uo (fname, pf)
  | Right mCons <- eCons = okReport mCons
  | Left exc    <- eCons = errReport exc
  where
    -- Format report
    okReport Nothing     = logs ++ "\n" ++ fname ++ ": Consistent. " ++ show nVars ++ " variables checked."
    okReport (Just cons) = logs ++ "\n" ++ fname ++ ": Inconsistent:\n" ++ reportErrors cons ++ "\n\n" ++
                           unlines (map show constraints)

    reportErrors cons = unlines [ maybe "" showSS ss ++ str | (ss, str) <- reports ]
      where
        reports = map head . group . sort $ map reportError cons
        showSS  = (++ ": ") . (" - at "++) . showSrcSpan

    reportError con = (findCon con, pprintConstr (orient (unrename nameMap con)) ++ additionalInfo con)
      where
        -- Create additional info for errors
        additionalInfo con =
           if null (errorInfo con)
           then ""
           else "\n    instead" ++ intercalate "\n" (mapNotFirst (pad 10) (errorInfo con))
        -- Create additional info about inconsistencies involving variables
        errorInfo con =
            [" '" ++ sName ++ "' is '" ++ pprintUnitInfo (unrename nameMap u) ++ "'"
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
        mapNotFirst f (x : xs) =  x : (map f xs)

        orient (ConEq u (UnitVar v)) = ConEq (UnitVar v) u
        orient (ConEq u (UnitParamVarUse v)) = ConEq (UnitParamVarUse v) u
        orient c = c

        pad o = (++) (replicate o ' ')

        srcSpan con | Just ss <- findCon con = showSrcSpan ss ++ " "
                    | otherwise              = ""

    -- Find a given constraint within the annotated AST. FIXME: optimise

    findCon :: Constraint -> Maybe FU.SrcSpan
    findCon con = lookupWith (eq con) constraints
      where eq c1 c2 = or [ conParamEq c1 c2' | c2' <- universeBi c2 ]

    constraints = [ (c, FU.getSpan x) | x <- universeBi pfUA :: [F.Expression UA]  , c <- maybeToList (getConstraint x) ] ++
                  [ (c, FU.getSpan x) | x <- universeBi pfUA :: [F.Statement UA]   , c <- maybeToList (getConstraint x) ] ++
                  [ (c, FU.getSpan x) | x <- universeBi pfUA :: [F.Argument UA]    , c <- maybeToList (getConstraint x) ] ++
                  [ (c, FU.getSpan x) | x <- universeBi pfUA :: [F.Declarator UA]  , c <- maybeToList (getConstraint x) ] ++
                  -- Why reverse? So that PUFunction and PUSubroutine appear first in the list, before PUModule.
                  reverse [ (c, FU.getSpan x) | x <- universeBi pfUA :: [F.ProgramUnit UA]
                                              , c <- maybeToList (getConstraint x) ]

    varReport     = intercalate ", " . map showVar

    showVar (UnitVar (_, s)) = s
    showVar (UnitLiteral _)   = "<literal>" -- FIXME
    showVar _                 = "<bad>"

    errReport exc = logs ++ "\n" ++ fname ++ ":  " ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eCons, state, logs) = runUnitSolver uOpts pfRenamed $ do
      modifyTemplateMap . const . combinedTemplateMap . M.elems $ uoModFiles uo
      initInference
      runInconsistentConstraints
    templateMap = usTemplateMap state
    pfUA :: F.ProgramFile UA
    pfUA = usProgramFile state -- the program file after units analysis is done

    -- number of 'real' variables checked, e.g. not parametric
    nVars = M.size . M.filter (not . isParametricUnit) $ usVarUnitMap state
    isParametricUnit u = case u of UnitParamPosAbs {} -> True; UnitParamPosUse {} -> True
                                   UnitParamVarAbs {} -> True; UnitParamVarUse {} -> True
                                   _ -> False

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uo))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pfRenamed

lookupWith :: (a -> Bool) -> [(a,b)] -> Maybe b
lookupWith f = fmap snd . find (f . fst)

{-| Check and infer units-of-measure for a program
    This produces an output of all the unit information for a program -}
inferUnits uo (fname, pf)
  | Right []   <- eVars = checkUnits uo (fname, pf)
  | Right vars <- eVars = okReport vars
  | Left exc   <- eVars = errReport exc
  where
    -- Format report
    okReport vars = logs ++ "\n" ++ fname ++ ":\n" ++ unlines [ expReport ei | ei <- expInfo ]
      where
        expInfo = [ (e, u) | e <- declVariables pfUA
                           , u <- maybeToList ((FA.varName e, FA.srcName e) `lookup` vars) ]

    expReport (e, u) = "  " ++ showSrcSpan (FU.getSpan e) ++ " unit " ++ show u ++ " :: " ++ FA.srcName e

    errReport exc = logs ++ "\n" ++ fname ++ ":  " ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eVars, state, logs) = runUnitSolver uOpts pfRenamed $ do
      modifyTemplateMap . const . combinedTemplateMap . M.elems $ uoModFiles uo
      initInference
      runInferVariables

    pfUA = usProgramFile state -- the program file after units analysis is done

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uo))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap mkUnitAnnotation $ pf

    nameMap = FAR.extractNameMap pfRenamed

combinedTemplateMap :: ModFiles -> TemplateMap
combinedTemplateMap = M.unions . map mfTemplateMap

-- | Name of the labeled data within a ModFile containing unit-specific info.
unitsTemplateMapLabel = "units-template-map"

mfTemplateMap :: ModFile -> TemplateMap
mfTemplateMap mf = case lookupModFileData unitsTemplateMapLabel mf of
  Nothing -> M.empty
  Just bs -> case decodeOrFail (LB.fromStrict bs) of
    Left _ -> M.empty
    Right (_, _, tmap) -> tmap

genUnitsModFile :: F.ProgramFile UA -> TemplateMap -> ModFile
genUnitsModFile pf tmap = alterModFileData f unitsTemplateMapLabel $ genModFile pf
  where
    f _ = Just . LB.toStrict $ encode tmap

compileUnits :: UnitOpts -> [FileProgram] -> (String, [(Filename, B.ByteString)])
compileUnits uo fileprogs = (concat reports, concat bins)
  where
    (reports, bins) = unzip [ (report, bin) | fileprog <- fileprogs
                                            , let (report, bin) = compileUnits' uo fileprog ]

compileUnits' :: UnitOpts -> FileProgram -> (String, [(Filename, B.ByteString)])
compileUnits' uo (fname, pf)
  | Right tmap <- eTMap = okReport tmap
  | Left exc   <- eTMap = errReport exc
  where
    -- Format report
    okReport tmap = ( logs ++ "\n" ++ fname ++ ":\n" ++ unlines [ n ++ ":\n  " ++ intercalate "\n  " (map show cs) | (n, cs) <- M.toList tmap ]
                     -- FIXME, filename manipulation
                    , [(fname ++ modFileSuffix, encodeModFile (genUnitsModFile pfTyped tmap))] )

    errReport exc = ( logs ++ "\n" ++ fname ++ ":  " ++ show exc
                    , [] )

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eTMap, state, logs) = runUnitSolver uOpts pfTyped $ do
      modifyTemplateMap . const . combinedTemplateMap . M.elems $ uoModFiles uo
      initInference
      runCompileUnits

    pfUA = usProgramFile state -- the program file after units analysis is done

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uo))
    tenv = combinedTypeEnv (M.elems (uoModFiles uo))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    pfTyped = fst . FAT.analyseTypesWithEnv tenv $ pfRenamed

    nameMap = FAR.extractNameMap pfTyped

synthesiseUnits :: UnitOpts
                -> Char
                -> (Filename, F.ProgramFile Annotation)
                -> (Report, (Filename, F.ProgramFile Annotation))
{-| Synthesis unspecified units for a program (after checking) -}
synthesiseUnits uo marker (fname, pf)
  | Right []   <- eVars = (checkUnits uo (fname, pf), (fname, pf))
  | Right vars <- eVars = (okReport vars, (fname, pfFinal))
  | Left exc   <- eVars = (errReport exc, (fname, pfFinal))
  where
    -- Format report
    okReport vars = logs ++ "\n" ++ fname ++ ":\n" ++ unlines [ expReport ei | ei <- expInfo ]
      where
        expInfo = [ (e, u) | e <- declVariables pfUA
                           , u <- maybeToList ((FA.varName e, FA.srcName e) `lookup` vars) ]

    expReport (e, u) = "  " ++ showSrcSpan (FU.getSpan e) ++ " unit " ++ show u ++ " :: " ++ FA.srcName e

    errReport exc = logs ++ "\n" ++ fname ++ ":  " ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eVars, state, logs) = runUnitSolver uOpts pfRenamed $ do
      modifyTemplateMap . const . combinedTemplateMap . M.elems $ uoModFiles uo
      initInference
      runInferVariables >>= runSynthesis marker

    pfUA = usProgramFile state -- the program file after units analysis is done
    pfFinal = fmap prevAnnotation . fmap FA.prevAnnotation $ pfUA -- strip annotations

    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (M.elems (uoModFiles uo))
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pfRenamed

--------------------------------------------------

unrename nameMap = transformBi $ \ x -> x `fromMaybe` M.lookup x nameMap

showSrcSpan :: FU.SrcSpan -> String
showSrcSpan (FU.SrcSpan l u) = show l

declVariables :: F.ProgramFile UA -> [F.Expression UA]
declVariables pf = flip mapMaybe (universeBi pf) $ \ d -> case d of
  F.DeclVariable _ _ v@(F.ExpValue _ _ (F.ValVariable _)) _ _   -> Just v
  F.DeclArray    _ _ v@(F.ExpValue _ _ (F.ValVariable _)) _ _ _ -> Just v
  _                                                             -> Nothing
