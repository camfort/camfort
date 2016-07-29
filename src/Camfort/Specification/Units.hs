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
import Data.Maybe (fromMaybe, maybeToList, listToMaybe)
import Data.Generics.Uniplate.Operations
import Control.Monad.State.Strict

import Camfort.Helpers hiding (lineCol)
import Camfort.Output
import Camfort.Analysis.Annotations
import Camfort.Analysis.Syntax
import Camfort.Analysis.Types
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

inferCriticalVariables, checkUnits, inferUnits, synthesiseUnits
  :: UnitOpts -> (Filename, F.ProgramFile Annotation) -> (Report, (Filename, F.ProgramFile Annotation))

{-| Infer one possible set of critical variables for a program -}
inferCriticalVariables uo (fname, pf)
  | Right vars <- eVars = (okReport vars, (fname, pf))
  | Left exc   <- eVars = (errReport exc, (fname, pf))
  where
    -- Format report
    okReport []   = logs ++ "\n" ++ fname ++ ":\n" ++ "No additional annotations are necessary.\n"
    okReport vars = logs ++ "\n" ++ fname ++ ":\n" ++ unlines [ "  " ++ expReport ei | ei <- expInfo ]
      where
        names = map showVar vars
        expInfo = [ e | s@(F.StDeclaration {})               <- universeBi pfUA :: [F.Statement UA]
                      , e@(F.ExpValue _ _ (F.ValVariable _)) <- universeBi s    :: [F.Expression UA]
                      , FA.varName e `elem` names ]

    expReport e = showSrcSpan (FU.getSpan e) ++ " " ++ unrename nameMap v
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

{-| Check units-of-measure for a program -}
checkUnits uo (fname, pf)
  | Right mCons <- eCons = (okReport mCons, (fname, pf))
  | Left exc    <- eCons = (errReport exc, (fname, pf))
  where
    -- Format report
    okReport Nothing = logs ++ "\n" ++ fname ++ ": Consistent. " ++ show nVars ++ " variables checked.\n"
    okReport (Just cons) = logs ++ "\n" ++ fname ++ ": Inconsistent:\n" ++ reportErrors cons

    reportErrors cons = unlines [ reportError con | con <- cons ]
    reportError con = "  " ++ srcSpan con
                      ++ pprintConstr (unrename nameMap con)
                      ++ additionalInfo con
      where
        -- Create additional info for errors
        additionalInfo con =
           if null (errorInfo con)
           then ""
           else "\n" ++ pad 2 ++ "where\n" ++ unlines (errorInfo con)
        -- Create additional info about inconsistencies involving variables
        errorInfo con =
            [pad 4 ++ (unrename nameMap v) ++ " === " ++ pprintUnitInfo (unrename nameMap u)
              | (v, Just u) <-
                  [(v, M.lookup v varUnitMap) | UnitVar v <- universeBi con ]]

        pad o = replicate (3 + o + length (srcSpan con)) ' '
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

    errReport exc = logs ++ "\n" ++ fname ++ ": " ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eCons, state, logs) = runUnitSolver uOpts pfRenamed $ initInference >> runInconsistentConstraints
    varUnitMap = usVarUnitMap state
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
  | Right vars <- eVars = (okReport vars, (fname, pf))
  | Left exc   <- eVars = (errReport exc, (fname, pf))
  where
    -- Format report
    okReport vars = logs ++ "\n" ++ fname ++ ":\n" ++ unlines [ expReport ei | ei <- expInfo ]
      where
        expInfo = [ (e, u) | s@(F.StDeclaration {})               <- universeBi pfUA :: [F.Statement UA]
                           , e@(F.ExpValue _ _ (F.ValVariable _)) <- universeBi s    :: [F.Expression UA]
                           , u <- maybeToList (FA.varName e `lookup` vars) ]

    expReport (e, u) = "  " ++ showSrcSpan (FU.getSpan e) ++ " unit " ++ show u ++ " :: " ++ unrename nameMap v
      where v = FA.varName e

    errReport exc = logs ++ "\n" ++ fname ++ ": " ++ show exc 

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eVars, state, logs) = runUnitSolver uOpts pfRenamed $ initInference >> runInferVariables

    pfUA = usProgramFile state -- the program file after units analysis is done

    pfRenamed = FAR.analyseRenames . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pfRenamed

{-| Synthesis unspecified units for a program (after checking) -}
synthesiseUnits uo (fname, pf)
  | Right []   <- eVars = checkUnits uo (fname, pf)
  | Right vars <- eVars = (okReport vars, (fname, pfFinal))
  | Left exc   <- eVars = (errReport exc, (fname, pfFinal))
  where
    -- Format report
    okReport vars = logs ++ "\n" ++ fname ++ ":\n" ++ unlines [ expReport ei | ei <- expInfo ]
      where
        expInfo = [ (e, u) | s@(F.StDeclaration {})               <- universeBi pfUA :: [F.Statement UA]
                           , e@(F.ExpValue _ _ (F.ValVariable _)) <- universeBi s    :: [F.Expression UA]
                           , u <- maybeToList (FA.varName e `lookup` vars) ]

    expReport (e, u) = "  " ++ showSrcSpan (FU.getSpan e) ++ " unit " ++ show u ++ " :: " ++ (v `fromMaybe` M.lookup v nameMap)
      where v = FA.varName e

    errReport exc = logs ++ "\n" ++ fname ++ ": " ++ show exc

    -- run inference
    uOpts = uo { uoNameMap = nameMap }
    (eVars, state, logs) = runUnitSolver uOpts pfRenamed $ initInference >> runInferVariables >>= runSynthesis

    pfUA = usProgramFile state -- the program file after units analysis is done
    pfFinal = fmap prevAnnotation . fmap FA.prevAnnotation $ pfUA -- strip annotations

    pfRenamed = FAR.analyseRenames . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pfRenamed

--------------------------------------------------

unrename nameMap = transformBi $ \ x -> x `fromMaybe` M.lookup x nameMap

showSrcSpan :: FU.SrcSpan -> String
showSrcSpan (FU.SrcSpan l u) = show l
