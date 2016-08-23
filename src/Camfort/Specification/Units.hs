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
  (checkUnits, inferUnits, synthesiseUnits, inferCriticalVariables)
where

import qualified Data.Map.Strict as M
import Data.Char (isNumber)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, maybeToList, listToMaybe, mapMaybe)
import Data.Generics.Uniplate.Operations
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
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Util.Position as FU

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
    okReport []   = (logs ++ "\n" ++ fname ++ ":\n"
                         ++ "No additional annotations are necessary.\n", 0)
    okReport vars = (logs ++ "\n" ++ fname ++ ": "
                         ++ show numVars
                         ++ " variable declarations suggested to be given a specification:\n"
                         ++ unlines [ "\t" ++ expReport ei | ei <- expInfo ], numVars)
      where
        names = map showVar vars
        expInfo = filter ((`elem` names) . FA.varName) $ declVariables pfUA
        numVars = length expInfo

    expReport e = "(" ++ showSrcSpan (FU.getSpan e) ++ ")\t" ++ unrename nameMap v
      where v = FA.varName e

    varReport     = intercalate ", " . map showVar

    showVar (UnitVar v)     = v
    showVar (UnitLiteral _) = "<literal>"
    showVar _               = "<bad>"

    errReport exc = logs ++ "\n" ++ fname ++ ":\n" ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eVars, state, logs) = runUnitSolver uOpts pfRenamed $ initInference >> runCriticalVariables
    pfUA = usProgramFile state -- the program file after units analysis is done

    pfRenamed = FAR.analyseRenames . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pfRenamed

checkUnits, inferUnits
            :: UnitOpts -> (Filename, F.ProgramFile Annotation) -> Report
{-| Check units-of-measure for a program -}
checkUnits uo (fname, pf)
  | Right mCons <- eCons = okReport mCons
  | Left exc    <- eCons = errReport exc
  where
    -- Format report
    okReport Nothing = logs ++ "\n" ++ fname ++ "\t: Consistent. " ++ show nVars ++ " variables checked."
    okReport (Just cons) = logs ++ "\n" ++ fname ++ "\t: Inconsistent:\n" ++ reportErrors cons

    reportErrors cons = unlines [ reportError con | con <- cons ]
    reportError con = " - at " ++ srcSpan con
                      ++ pprintConstr (orient (unrename nameMap con))
                      ++ additionalInfo con
      where
        -- Create additional info for errors
        additionalInfo con =
           if null (errorInfo con)
           then ""
           else "\n    instead" ++ intercalate "\n" (mapNotFirst (pad 10) (errorInfo con))
        -- Create additional info about inconsistencies involving variables
        errorInfo con =
            [" '" ++ (unrename nameMap v) ++ "' is '" ++ pprintUnitInfo (unrename nameMap u) ++ "'"
              | UnitVar v <- universeBi con
              ,         u <- findUnitConstrFor con v ]
        -- Find unit information for variable constraints
        findUnitConstrFor con v = mapMaybe (\con' -> if con == con'
                                                     then Nothing
                                                     else constrainedTo v con')
                                           (concat $ M.elems templateMap)
        constrainedTo v (ConEq (UnitVar v') u) | v == v' = Just u
        constrainedTo v (ConEq u (UnitVar v')) | v == v' = Just u
        constrainedTo _ _ = Nothing

        mapNotFirst f [] = []
        mapNotFirst f (x : xs) =  x : (map f xs)

        orient (ConEq u (UnitVar v)) = ConEq (UnitVar v) u
        orient c = c

        pad o = (++) (replicate o ' ')

        srcSpan con | Just ss <- findCon con = showSrcSpan ss ++ " "
                    | otherwise              = ""

    -- Find a given constraint within the annotated AST. FIXME: optimise
    findCon :: Constraint -> Maybe FU.SrcSpan
    findCon con = listToMaybe $ [ FU.getSpan x | x <- universeBi pfUA :: [F.Expression UA], getConstraint x `eq` con ] ++
                                [ FU.getSpan x | x <- universeBi pfUA :: [F.Statement UA] , getConstraint x `eq` con ] ++
                                [ FU.getSpan x | x <- universeBi pfUA :: [F.Declarator UA], getConstraint x `eq` con ] ++
                                [ FU.getSpan x | x <- universeBi pfUA :: [F.Argument UA]  , getConstraint x `eq` con ]
      where eq Nothing _    = False
            eq (Just c1) c2 = conParamEq c1 c2

    varReport     = intercalate ", " . map showVar

    showVar (UnitVar v)     = v `fromMaybe` M.lookup v nameMap
    showVar (UnitLiteral _) = "<literal>" -- FIXME
    showVar _               = "<bad>"

    errReport exc = logs ++ "\n" ++ fname ++ ":\t " ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eCons, state, logs) = runUnitSolver uOpts pfRenamed $ initInference >> runInconsistentConstraints
    templateMap = usTemplateMap state
    pfUA :: F.ProgramFile UA
    pfUA = usProgramFile state -- the program file after units analysis is done

    -- number of 'real' variables checked, e.g. not parametric
    nVars = M.size . M.filter (not . isParametricUnit) $ usVarUnitMap state
    isParametricUnit u = case u of UnitParamPosAbs {} -> True; UnitParamPosUse {} -> True
                                   UnitParamVarAbs {} -> True; UnitParamVarUse {} -> True
                                   _ -> False

    pfRenamed = FAR.analyseRenames . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pfRenamed

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
                           , u <- maybeToList (FA.varName e `lookup` vars) ]

    expReport (e, u) = "  " ++ showSrcSpan (FU.getSpan e) ++ " unit " ++ show u ++ " :: " ++ unrename nameMap v
      where v = FA.varName e

    errReport exc = logs ++ "\n" ++ fname ++ ":\t" ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eVars, state, logs) = runUnitSolver uOpts pfRenamed $ initInference >> runInferVariables

    pfUA = usProgramFile state -- the program file after units analysis is done

    pfRenamed = FAR.analyseRenames . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pfRenamed

synthesiseUnits :: UnitOpts
                -> Bool
                -> (Filename, F.ProgramFile Annotation)
                -> (Report, (Filename, F.ProgramFile Annotation))
{-| Synthesis unspecified units for a program (after checking) -}
synthesiseUnits uo doxygenEnabled (fname, pf)
  | Right []   <- eVars = (checkUnits uo (fname, pf), (fname, pf))
  | Right vars <- eVars = (okReport vars, (fname, pfFinal))
  | Left exc   <- eVars = (errReport exc, (fname, pfFinal))
  where
    -- Format report
    okReport vars = logs ++ "\n" ++ fname ++ ":\n" ++ unlines [ expReport ei | ei <- expInfo ]
      where
        expInfo = [ (e, u) | e <- declVariables pfUA
                           , u <- maybeToList (FA.varName e `lookup` vars) ]

    expReport (e, u) = "  " ++ showSrcSpan (FU.getSpan e) ++ " unit " ++ show u ++ " :: " ++ (v `fromMaybe` M.lookup v nameMap)
      where v = FA.varName e

    errReport exc = logs ++ "\n" ++ fname ++ ":\t" ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eVars, state, logs) = runUnitSolver uOpts pfRenamed $ initInference >> runInferVariables >>= runSynthesis doxygenEnabled

    pfUA = usProgramFile state -- the program file after units analysis is done
    pfFinal = fmap prevAnnotation . fmap FA.prevAnnotation $ pfUA -- strip annotations

    pfRenamed = FAR.analyseRenames . FA.initAnalysis . fmap mkUnitAnnotation $ pf
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
