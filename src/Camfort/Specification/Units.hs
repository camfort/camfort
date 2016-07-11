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

TODO:
 * Deal with variable shadowing in "contained" functions.
 * Better errors with line number info

-}


{-# LANGUAGE ScopedTypeVariables, ImplicitParams, DoAndIfThenElse, PatternGuards #-}

module Camfort.Specification.Units where


import Data.Data
import Data.Char (isNumber)
import Data.List
import Data.Label.Mono (Lens)
import qualified Data.Label
import Data.Label.Monadic hiding (modify)
import Data.Function
import Data.Matrix
import Control.Monad.State.Strict hiding (gets)
import Data.Generics.Uniplate.Operations

import Camfort.Helpers
import Camfort.Output
import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Analysis.Syntax
import Camfort.Analysis.Types

import Camfort.Specification.Units.Debug
import Camfort.Specification.Units.InferenceBackend
import Camfort.Specification.Units.InferenceFrontend
import Camfort.Specification.Units.Synthesis
import Camfort.Specification.Units.Strip

import Camfort.Specification.Units.SyntaxConversion
import Camfort.Specification.Units.Environment -- Provides the types and data accessors used in this module
import Camfort.Specification.Units.Solve -- Solvers for the Gaussian matrix

import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.AST as F
import Camfort.Transformation.Syntax

-- For debugging and development purposes
import qualified Debug.Trace as D

-- Helper that is require before running any of the units operations
modifyAST (f, inp, ast) = (f, ast)

{- START HERE! Two main functions of this file: inferUnits and removeUnits -}

removeUnits ::
    (Filename, F.ProgramFile Annotation) -> (Report, (Filename, F.ProgramFile Annotation))
removeUnits (fname, x) = undefined
  {-  let ?criticals = False
    in ("", (fname, map (descendBi removeUnitsInBlock) x)) -}


-- *************************************
--   Unit inference (top - level)
-- *************************************

{-| Infer one possible set of critical variables for a program -}
inferCriticalVariables ::
       (?solver :: Solver, ?assumeLiterals :: AssumeLiterals)
    => (Filename, F.ProgramFile Annotation)
    -> (Report, (Filename, F.ProgramFile Annotation))
inferCriticalVariables (fname, pf) = (r, (fname, pf))
  where
    -- Format report
    r = concat [fname ++ ": " ++ r ++ "\n" | r <- Data.Label.get report env]

    -- Run the infer procedure with empty unit environment, retunring
    -- the updated unit environment, matching variables to their units
    env = let ?criticals = True
              ?debug     = False
          in  execState infer emptyUnitEnv

    pf' = FAR.analyseRenames . FA.initAnalysis $ pf
    -- Core infer procedure
    infer :: (?criticals :: Bool, ?debug :: Bool) => State UnitEnv ()
    infer = do
        doInferUnits . FAB.analyseBBlocks $ pf'
        vars <- criticalVars
        case vars of
          [] -> do report <<++ "No critical variables. Appropriate annotations."
          _  -> do report <<++ "Critical variables: "
                            ++ (concat $ intersperse "," vars)
        ifDebug debugGaussian

{-| Infer/check the unspecified units for a program -}
inferUnits ::
       (?solver :: Solver, ?assumeLiterals :: AssumeLiterals)
    => (Filename, F.ProgramFile Annotation)
    -> (Report, (Filename, F.ProgramFile Annotation))
inferUnits (fname, pf) = (r, (fname, pf))
  where
    -- Format report
    r = concat [fname ++ ": " ++ r ++ "\n" | r <- Data.Label.get report env]
        ++ fname ++ ": checked/inferred " ++ show n ++ " user variables\n"
    -- Count number of checked and inferred variables
    n = countVariables (_varColEnv env) (_debugInfo env) (_procedureEnv env)
                                    (fst $ _linearSystem env) (_unitVarCats env)

    pf' = FAB.analyseBBlocks . FAR.analyseRenames . FA.initAnalysis $ pf
    -- Apply inferences
    env = let ?criticals = False
              ?debug     = False
          in execState (doInferUnits pf') emptyUnitEnv

{-| Synthesis unspecified units for a program (after checking) -}
synthesiseUnits ::
       (?solver :: Solver, ?assumeLiterals :: AssumeLiterals)
    => (Filename, F.ProgramFile Annotation)
    -> (Report, (Filename, F.ProgramFile Annotation))
synthesiseUnits (fname, pf) = (r, (fname, fmap FA.prevAnnotation pf3))
  where
    -- Format report
    r = concat [fname ++ ": " ++ r ++ "\n" | r <- Data.Label.get report env]
        ++ fname ++ ": checked/inferred " ++ show n ++ " user variables\n"
    -- Count number of checked and inferred variables
    n = countVariables (_varColEnv env) (_debugInfo env) (_procedureEnv env)
                                    (fst $ _linearSystem env) (_unitVarCats env)
    -- Apply inference and synthesis
    pf' = FAB.analyseBBlocks . FAR.analyseRenames . FA.initAnalysis $ pf
    (pf3, env) = runState inferAndSynthesise emptyUnitEnv
    inferAndSynthesise =
        let ?criticals = False
            ?debug     = False
        in do
          doInferUnits pf'
          succeeded <- gets success
          if succeeded
            then do
              p <- descendBiM insertUnitsInBlock pf'
              (n, added) <- gets evUnitsAdded
              report <<++ ("Added " ++ (show n) ++ " non-unitless annotation: "
                        ++ (concat $ intersperse " ," $ added))
              return p
            else return pf'

-- Count number of variables for which a spec has been checked
countVariables vars debugs procs matrix ucats =
    length $ filter (\c -> case (ucats !! (c - 1)) of
                             Variable -> case (lookupVarsByCols vars [c]) of
                                           [] -> False
                                           _  -> True
                             Argument -> case (lookupVarsByCols vars [c]) of
                                           [] -> False
                                           _  -> True
                             _        -> False) [1..ncols matrix]

-- Critical variables analysis
criticalVars :: State UnitEnv [String]
criticalVars = do uvarenv     <- gets varColEnv
                  (matrix, _) <- gets linearSystem
                  ucats       <- gets unitVarCats
                  dbgs        <- gets debugInfo
                  -- debugGaussian
                  let cv1 = criticalVars' uvarenv ucats matrix 1 dbgs
                  let cv2 = [] -- criticalVars
                  return (cv1 ++ cv2)

criticalVars' :: VarColEnv -> [UnitVarCategory] -> Matrix Rational -> Row -> DebugInfo -> [String]
criticalVars' varenv ucats matrix i dbgs =
  let m = firstNonZeroCoeff matrix ucats
  in
    if (i == nrows matrix) then
       if (m i) /= (ncols matrix) then
          lookupVarsByColsFilterByArg matrix varenv ucats [((m i) + 1)..(ncols matrix)] dbgs
       else []
    else
        if (m (i + 1)) /= ((m i) + 1)
        then (lookupVarsByColsFilterByArg matrix varenv ucats [((m i) + 1)..(m (i + 1) - 1)] dbgs) ++ (criticalVars' varenv ucats matrix (i + 1) dbgs)
        else criticalVars' varenv ucats matrix (i + 1) dbgs
