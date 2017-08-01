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
{-# LANGUAGE DeriveDataTypeable #-}
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

import           Control.Arrow (first, second)
import           Control.Monad
import qualified Data.Map.Strict as M
import           System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath  ((</>), takeDirectory)

import           Language.Fortran.Util.ModFile

import           Camfort.Analysis.Fortran
  (analysisDebug, analysisInput, analysisResult, branchAnalysis)
import           Camfort.Analysis.ModFile
  (genModFiles, readParseSrcDir, simpleCompiler)
import           Camfort.Analysis.Simple
import           Camfort.Helpers
import           Camfort.Input
import qualified Camfort.Specification.Stencils as Stencils
import           Camfort.Specification.Stencils.Analysis (compileStencils)
import qualified Camfort.Specification.Units as LU
import           Camfort.Specification.Units.Analysis (compileUnits)
import           Camfort.Specification.Units.Analysis.Consistent (checkUnits)
import           Camfort.Specification.Units.Analysis.Criticals  (inferCriticalVariables)
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


-- * Wrappers on all of the features
ast d excludes = do
    incDir <- getCurrentDirectory
    modFiles <- genModFiles simpleCompiler () incDir excludes
    xs <- readParseSrcDir modFiles d excludes
    print . fmap fst $ xs

countVarDecls inSrc excludes = do
    putStrLn $ "Counting variable declarations in '" ++ inSrc ++ "'"
    incDir <- getCurrentDirectory
    doAnalysisSummary countVariableDeclarations inSrc incDir excludes

dead inSrc excludes outSrc = do
    putStrLn $ "Eliminating dead code in '" ++ inSrc ++ "'"
    let rfun = do
          pfs <- analysisInput
          resA <- mapM (branchAnalysis $ deadCode False) pfs
          let (reports, results) = (fmap analysisDebug resA, fmap analysisResult resA)
          pure (mconcat reports, fmap (pure :: a -> Either () a) results)
    incDir <- getCurrentDirectory
    report <- doRefactorWithModFiles rfun simpleCompiler () inSrc incDir excludes outSrc
    putStrLn report

common inSrc excludes outSrc = do
    putStrLn $ "Refactoring common blocks in '" ++ inSrc ++ "'"
    isDir <- isDirectory inSrc
    let rfun = commonElimToModules (takeDirectory outSrc ++ "/")
    incDir <- getCurrentDirectory
    report <- doRefactorAndCreate rfun inSrc excludes incDir outSrc
    print report

equivalences inSrc excludes outSrc = do
    putStrLn $ "Refactoring equivalences blocks in '" ++ inSrc ++ "'"
    let rfun = do
          pfs <- analysisInput
          resA <- mapM (branchAnalysis refactorEquivalences) pfs
          let (reports, results) = (fmap analysisDebug resA, fmap analysisResult resA)
          pure (mconcat reports, fmap (pure :: a -> Either () a) results)
    incDir <- getCurrentDirectory
    report <- doRefactorWithModFiles rfun simpleCompiler () inSrc incDir excludes outSrc
    putStrLn report

{- Units feature -}
optsToUnitOpts :: LiteralsOpt -> Bool -> UnitOpts
optsToUnitOpts m debug = o1
  where o1 = unitOpts0 { uoLiterals = m
                       , uoDebug = debug
                       }

unitsCheck inSrc excludes m debug incDir = do
    putStrLn $ "Checking units for '" ++ inSrc ++ "'"
    let uo = optsToUnitOpts m debug
    incDir' <- maybe getCurrentDirectory pure incDir
    doAnalysisReportWithModFiles checkUnits compileUnits uo inSrc incDir' excludes

unitsInfer inSrc excludes m debug incDir = do
    putStrLn $ "Inferring units for '" ++ inSrc ++ "'"
    let uo = optsToUnitOpts m debug
    incDir' <- maybe getCurrentDirectory pure incDir
    doAnalysisReportWithModFiles LU.inferUnits compileUnits uo inSrc incDir' excludes

unitsSynth inSrc excludes m debug incDir outSrc annType = do
    putStrLn $ "Synthesising units for '" ++ inSrc ++ "'"
    let marker = markerChar annType
    let uo = optsToUnitOpts m debug
    let rfun = do
          pfs <- analysisInput
          results <- mapM (branchAnalysis (LU.synthesiseUnits marker)) pfs
          let normalizedResults =
                (\res -> ( show (analysisDebug res) ++ either show (show . fst) (analysisResult res)
                         , case analysisResult res of
                             Left err     -> Left err
                             Right (_,pf) -> Right pf)) <$> results
          pure . first concat $ unzip normalizedResults
    incDir' <- maybe getCurrentDirectory pure incDir
    report <- doRefactorWithModFiles rfun compileUnits uo inSrc incDir' excludes outSrc
    putStrLn report

unitsCriticals inSrc excludes m debug incDir = do
    putStrLn $ "Suggesting variables to annotate with unit specifications in '"
             ++ inSrc ++ "'"
    let uo = optsToUnitOpts m debug
    incDir' <- maybe getCurrentDirectory pure incDir
    doAnalysisReportWithModFiles inferCriticalVariables compileUnits uo inSrc incDir' excludes

{- Stencils feature -}
stencilsCheck inSrc excludes = do
   putStrLn $ "Checking stencil specs for '" ++ inSrc ++ "'"
   incDir <- getCurrentDirectory
   doAnalysisSummary Stencils.check inSrc incDir excludes

stencilsInfer inSrc excludes useEval = do
   putStrLn $ "Inferring stencil specs for '" ++ inSrc ++ "'"
   let rfun = Stencils.infer useEval '='
   incDir <- getCurrentDirectory
   doAnalysisSummary rfun inSrc incDir excludes

stencilsSynth inSrc excludes annType outSrc = do
   putStrLn $ "Synthesising stencil specs for '" ++ inSrc ++ "'"
   let rfun = second (fmap (pure :: a -> Either () a)) <$> Stencils.synth (markerChar annType)
   incDir <- getCurrentDirectory
   report <- doRefactorWithModFiles rfun compileStencils () inSrc incDir excludes outSrc
   putStrLn report

-- | Initialize Camfort for the given project.
camfortInitialize :: FilePath -> IO ()
camfortInitialize projectDir =
  createDirectoryIfMissing False (projectDir </> ".camfort")
