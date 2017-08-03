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

{- This module collects together stubs that connect analysis/transformations
   with the input -> output procedures -}

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camfort.Functionality (
  -- * Datatypes
    AnnotationType(..)
  -- * Commands
  , ast
  , countVarDecls
  -- ** Stencil Analysis
  , stencilsCheck
  , stencilsInfer
  , stencilsSynth
  -- ** Unit Analysis
  , unitsCriticals
  , unitsCheck
  , unitsInfer
  , unitsSynth
  -- ** Refactorings
  , common
  , dead
  , equivalences
  -- ** Project Management
  , camfortInitialize
  ) where

import Control.Arrow (first, second)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath  ((</>), takeDirectory)

import           Camfort.Analysis
  (analysisDebug, analysisInput, analysisResult, branchAnalysis)
import           Camfort.Analysis.Annotations (mkReport)
import           Camfort.Analysis.ModFile
  (genModFiles, readParseSrcDir, simpleCompiler)
import           Camfort.Analysis.Simple
import           Camfort.Input
import qualified Camfort.Specification.Stencils as Stencils
import           Camfort.Specification.Stencils.Analysis (compileStencils)
import qualified Camfort.Specification.Units as LU
import           Camfort.Specification.Units.Analysis (compileUnits)
import           Camfort.Specification.Units.Analysis.Consistent (checkUnits)
import           Camfort.Specification.Units.Analysis.Criticals  (inferCriticalVariables)
import           Camfort.Specification.Units.Analysis.Infer      (inferUnits)
import           Camfort.Specification.Units.Monad
import           Camfort.Transformation.CommonBlockElim
import           Camfort.Transformation.DeadCode
import           Camfort.Transformation.EquivalenceElim

data AnnotationType = ATDefault | Doxygen | Ford


-- | Retrieve the marker character compatible with the given
-- type of annotation.
markerChar :: AnnotationType -> Char
markerChar Doxygen   = '<'
markerChar Ford      = '!'
markerChar ATDefault = '='

runFunctionality description runner analysis compiler env inSrc incDir excludes = do
  putStrLn $ description ++ " '" ++ inSrc ++ "'"
  incDir' <- maybe getCurrentDirectory pure incDir
  runner analysis compiler env inSrc incDir' excludes

withOutRunner runner outSrc analysis compiler env inSrc incDir excludes
  = runner analysis compiler env inSrc incDir excludes outSrc

-- * Wrappers on all of the features
ast d incDir excludes = do
    incDir' <- maybe getCurrentDirectory pure incDir
    modFiles <- genModFiles simpleCompiler () incDir' excludes
    xs <- readParseSrcDir modFiles d excludes
    print . fmap fst $ xs

countVarDecls =
  runFunctionality "Counting variable declarations in"
    doAnalysisReport countVariableDeclarations simpleCompiler ()

dead inSrc incDir excludes outSrc =
  let rfun = do
        pfs <- analysisInput
        resA <- mapM (branchAnalysis $ deadCode False) pfs
        let (reports, results) = (fmap analysisDebug resA, fmap analysisResult resA)
        pure (mconcat reports, fmap (pure :: a -> Either () a) results)
  in runFunctionality "Eliminating dead code in" (withOutRunner doRefactor outSrc)
     rfun simpleCompiler () inSrc incDir excludes

common inSrc incDir excludes outSrc =
  let rfun = commonElimToModules (takeDirectory outSrc ++ "/")
  in runFunctionality "Refactoring common blocks in"
     (withOutRunner doRefactorAndCreate outSrc) rfun simpleCompiler () inSrc incDir excludes

equivalences inSrc incDir excludes outSrc = do
  let rfun = do
        pfs <- analysisInput
        resA <- mapM (branchAnalysis refactorEquivalences) pfs
        let (reports, results) = (fmap analysisDebug resA, fmap analysisResult resA)
        pure (mconcat reports, fmap (pure :: a -> Either () a) results)
  runFunctionality "Refactoring equivalences blocks in"
    (withOutRunner doRefactor outSrc) rfun simpleCompiler () inSrc incDir excludes

{- Units feature -}

runUnitsFunctionality description runner analysis inSrc incDir excludes m debug =
  let uo = optsToUnitOpts m debug
  in runFunctionality description runner analysis compileUnits uo inSrc incDir excludes

optsToUnitOpts :: LiteralsOpt -> Bool -> UnitOpts
optsToUnitOpts m debug = o1
  where o1 = unitOpts0 { uoLiterals = m
                       , uoDebug = debug
                       }

unitsCheck = runUnitsFunctionality "Checking units for" doAnalysisReport checkUnits

unitsInfer = runUnitsFunctionality "Inferring units for" doAnalysisReport inferUnits'
  where inferUnits' = (mkReport . either show show) <$> inferUnits

unitsSynth inSrc incDir excludes m debug outSrc annType =
  let marker = markerChar annType
      rfun = do
        pfs <- analysisInput
        results <- mapM (branchAnalysis (LU.synthesiseUnits marker)) pfs
        let normalizedResults =
              (\res -> ( show (analysisDebug res) ++ either show (show . fst) (analysisResult res)
                       , case analysisResult res of
                           Left err     -> Left err
                           Right (_,pf) -> Right pf)) <$> results
        pure . first concat $ unzip normalizedResults
      runner' = withOutRunner doRefactor outSrc
  in runUnitsFunctionality "Synthesising units for" runner' rfun inSrc incDir excludes m debug

unitsCriticals =
  runUnitsFunctionality "Suggesting variables to annotate with unit specifications in"
    doAnalysisReport inferCriticalVariables

{- Stencils feature -}

runStencilsFunctionality description runner analysis =
  runFunctionality description runner analysis compileStencils ()

stencilsCheck =
  runStencilsFunctionality "Checking stencil specs for" doAnalysisReport Stencils.check

stencilsInfer inSrc incDir excludes useEval =
  let rfun = Stencils.infer useEval '='
  in runStencilsFunctionality "Inferring stencil specs for" doAnalysisReport rfun inSrc incDir excludes

stencilsSynth inSrc incDir excludes annType outSrc =
  let rfun = second (fmap (pure :: a -> Either () a)) <$> Stencils.synth (markerChar annType)
      runner' = withOutRunner doRefactor outSrc
  in runStencilsFunctionality "Synthesising stencil specs for" runner' rfun inSrc incDir excludes

-- | Initialize Camfort for the given project.
camfortInitialize :: FilePath -> IO ()
camfortInitialize projectDir =
  createDirectoryIfMissing False (projectDir </> ".camfort")
