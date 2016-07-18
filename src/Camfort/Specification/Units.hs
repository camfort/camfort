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


{-# LANGUAGE ScopedTypeVariables, ImplicitParams, DoAndIfThenElse,
             PatternGuards, ConstraintKinds #-}

module Camfort.Specification.Units
          (Solver, removeUnits, checkUnits
                 , inferUnits, synthesiseUnits
                 , inferCriticalVariables)  where


import Data.Data
import Data.Char (isNumber)
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Label.Mono (Lens)
import qualified Data.Label
import Data.Label.Monadic hiding (modify)
import Data.Function
import Data.Matrix
import Data.Generics.Uniplate.Operations
import Control.Monad.State.Strict hiding (gets)
import Control.Monad.Identity

import Camfort.Helpers
import Camfort.Output
import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Analysis.Syntax
import Camfort.Analysis.Types

import Camfort.Input
import Camfort.Specification.Units.Debug
import Camfort.Specification.Units.Monad
import Camfort.Specification.Units.InferenceBackend
import Camfort.Specification.Units.InferenceFrontend
import qualified Camfort.Specification.Units.Synthesis as US
import Camfort.Specification.Units.Strip

-- Provides the types and data accessors used in this module
import Camfort.Specification.Units.Environment
-- Solvers for the Gaussian matrix
import Camfort.Specification.Units.Solve

import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.AST as F
import Camfort.Transformation.Syntax

-- For debugging and development purposes
import qualified Debug.Trace as D

------------------------------------------------
-- Set the default options for the inference

instance Default Solver where
    defaultValue = Custom
instance Default AssumeLiterals where
    defaultValue = Poly

{- START HERE! Two main functions of this file: inferUnits and removeUnits -}

-- Deprecated
removeUnits ::
    (Filename, F.ProgramFile Annotation) -> (Report, (Filename, F.ProgramFile Annotation))
removeUnits (fname, x) = undefined
  {-  let ?criticals = False
    in ("", (fname, map (descendBi removeUnitsInBlock) x)) -}


-- *************************************
--   Unit inference (top - level)
--
-- *************************************

type Params = (?solver :: Solver, ?assumeLiterals :: AssumeLiterals)

{-| Infer one possible set of critical variables for a program -}
inferCriticalVariables :: (Filename, F.ProgramFile Annotation) -> (Report, (Filename, F.ProgramFile Annotation))
inferCriticalVariables (fname, pf)
  | Right vars <- eVars = (okReport vars, (fname, pf))
  | Left exc   <- eVars = (errReport exc, (fname, pf))
  where
    -- Format report
    okReport vars = fname ++ ": " ++ varReport vars ++ "\n" ++ logs
    varReport     = intercalate ", " . map showVar

    showVar (Undetermined v)    = v `fromMaybe` M.lookup v nameMap
    showVar (UndeterminedLit _) = "<literal>" -- FIXME
    showVar _                   = "<bad>"

    errReport exc = fname ++ ": " ++ show exc ++ "\n" ++ logs

    -- run inference
    uOpts = UnitOpts { uoDebug          = False
                     , uoLiterals       = LitMixed
                     , uoNameMap        = nameMap
                     , uoArgumentDecls  = False }
    (eVars, state, logs) = runUnitSolver uOpts pf' $ runInference criticalVariables

    pf' = FAR.analyseRenames . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pf'

{-| Check units-of-measure for a program -}
checkUnits :: (Filename, F.ProgramFile Annotation) -> (Report, (Filename, F.ProgramFile Annotation))
checkUnits (fname, pf)
  | Right mCons <- eCons = (okReport mCons, (fname, pf))
  | Left exc    <- eCons = (errReport exc, (fname, pf))
  where
    -- Format report
    okReport Nothing = fname ++ ": Consistent.\n" ++ logs
    -- FIXME: better unit pretty printer
    okReport (Just cons) = fname ++ ": " ++ show cons ++ "\n" ++ logs
    varReport     = intercalate ", " . map showVar

    showVar (Undetermined v)    = v `fromMaybe` M.lookup v nameMap
    showVar (UndeterminedLit _) = "<literal>" -- FIXME
    showVar _                   = "<bad>"

    errReport exc = fname ++ ": " ++ show exc ++ "\n" ++ logs

    -- run inference
    uOpts = UnitOpts { uoDebug          = False
                     , uoLiterals       = LitMixed
                     , uoNameMap        = nameMap
                     , uoArgumentDecls  = False }
    (eCons, state, logs) = runUnitSolver uOpts pf' $ runInference inconsistentConstraints

    pf' = FAR.analyseRenames . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pf'

    -- FIXME:
    -- Count number of checked and inferred variables
    -- n = countVariables (_varColEnv env) (_debugInfo env) (_procedureEnv env)
    --                                 (fst $ _linearSystem env) (_unitVarCats env)

{-| Check and infer units-of-measure for a program
     This produces an output of all the unit information for a program -}
inferUnits ::
       Params
    => (Filename, F.ProgramFile Annotation)
    -> (Report, (Filename, F.ProgramFile Annotation))
inferUnits (fname, pf)
  | Right vars <- eVars = (okReport vars, (fname, pf))
  | Left exc   <- eVars = (errReport exc, (fname, pf))
  where
    -- Format report
    okReport vars = fname ++ ": " ++ varReport vars ++ "\n" ++ logs
    varReport     = intercalate ", " . map showVar

    showVar (v, info) = (v `fromMaybe` M.lookup v nameMap) ++ " :: " ++ show info

    errReport exc = fname ++ ": " ++ show exc ++ "\n" ++ logs

    -- run inference
    uOpts = UnitOpts { uoDebug          = False
                     , uoLiterals       = LitMixed
                     , uoNameMap        = nameMap
                     , uoArgumentDecls  = False }
    (eVars, state, logs) = runUnitSolver uOpts pf' $ runInference inferVariables

    pf' = FAR.analyseRenames . FA.initAnalysis . fmap mkUnitAnnotation $ pf
    nameMap = FAR.extractNameMap pf'


{-| Synthesis unspecified units for a program (after checking) -}
synthesiseUnits ::
       Params
    => (Filename, F.ProgramFile Annotation)
    -> (Report, (Filename, F.ProgramFile Annotation))
synthesiseUnits (fname, pf) = (r, (fname, fmap (prevAnnotation . FA.prevAnnotation) pf3))
  where
    -- Format report
    r = concat [fname ++ ": " ++ r ++ "\n" | r <- Data.Label.get report env, not (null r)]
        ++ fname ++ ": checked/inferred " ++ show n ++ " user variables\n"
    -- Count number of checked and inferred variables
    n = countVariables (_varColEnv env) (_debugInfo env) (_procedureEnv env)
                                    (fst $ _linearSystem env) (_unitVarCats env)
    -- Apply inference and synthesis
    pf' = FAB.analyseBBlocks . FAR.analyseRenames . FA.initAnalysis $ (fmap mkUnitAnnotation pf)
    (pf3, env) = runState inferAndSynthesise emptyUnitEnv
    nameMap = FAR.extractNameMap pf'
    inferAndSynthesise =
        let ?criticals = False
            ?debug     = False
            ?nameMap   = nameMap
            ?argumentDecls = False
        in do
          doInferUnits pf'
          succeeded <- gets success
          if succeeded
            then do
              p <- US.synthesiseUnits False pf'
              (n, _) <- gets evUnitsAdded
              report <<++ ("Added " ++ (show n) ++ " annotations")
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
