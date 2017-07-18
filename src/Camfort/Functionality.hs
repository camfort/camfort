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
  , unitsCompile
  -- ** Refactorings
  , common
  , dead
  , equivalences
  -- ** Project Management
  , camfortInitialize
  ) where

import Control.Arrow (second)
import Control.Monad
import System.Directory (createDirectoryIfMissing)
import System.FilePath  ((</>), takeDirectory)

import Camfort.Analysis (runRefactoring)
import Camfort.Analysis.ModFile (getModFiles)
import Camfort.Analysis.Simple
import Camfort.Transformation.DeadCode
import Camfort.Transformation.CommonBlockElim
import Camfort.Transformation.EquivalenceElim

import qualified Camfort.Specification.Units as LU
import Camfort.Specification.Units.Monad

import Camfort.Helpers
import Camfort.Input

import Language.Fortran.Util.ModFile
import qualified Camfort.Specification.Stencils as Stencils
import qualified Data.Map.Strict as M

data AnnotationType = ATDefault | Doxygen | Ford


-- | Retrieve the marker character compatible with the given
-- type of annotation.
markerChar :: AnnotationType -> Char
markerChar Doxygen   = '<'
markerChar Ford      = '!'
markerChar ATDefault = '='


-- * Wrappers on all of the features
ast d excludes = do
    xs <- readParseSrcDir d excludes
    print . fmap fst $ xs

countVarDecls inSrc excludes = do
    putStrLn $ "Counting variable declarations in '" ++ inSrc ++ "'"
    doAnalysisSummary countVariableDeclarations inSrc excludes

dead inSrc excludes outSrc = do
    putStrLn $ "Eliminating dead code in '" ++ inSrc ++ "'"
    report <- doRefactor (mapM (second (pure :: a -> Either () a) . deadCode False)) id inSrc excludes outSrc
    putStrLn report

common inSrc excludes outSrc = do
    putStrLn $ "Refactoring common blocks in '" ++ inSrc ++ "'"
    isDir <- isDirectory inSrc
    let rfun = commonElimToModules (takeDirectory outSrc ++ "/")
    report <- doRefactorAndCreate rfun inSrc excludes outSrc
    putStrLn report

equivalences inSrc excludes outSrc = do
    putStrLn $ "Refactoring equivalences blocks in '" ++ inSrc ++ "'"
    report <- doRefactor (mapM (second (pure :: a -> Either () a) . refactorEquivalences)) id inSrc excludes outSrc
    putStrLn report

{- Units feature -}
optsToUnitOpts :: LiteralsOpt -> Bool -> Maybe String -> IO UnitOpts
optsToUnitOpts m debug = maybe (pure o1)
  (fmap (\modFiles -> o1 { uoModFiles = modFiles }) . getModFiles)
  where o1 = unitOpts0 { uoLiterals = m
                       , uoDebug = debug
                       , uoModFiles = emptyModFiles }

printUnitsResults :: (Show e, Show a) => [(String, Either e a)] -> IO ()
printUnitsResults = mapM_ printResults
  where printResults (report, Left e)  = putStrLn (report ++ show e)
        printResults (report, Right r) = putStrLn (report ++ show r)

unitsCheck inSrc excludes m debug incDir = do
    putStrLn $ "Checking units for '" ++ inSrc ++ "'"
    uo <- optsToUnitOpts m debug incDir
    let rfun = LU.checkUnits uo
    doAnalysisReportWithModFiles rfun (printUnitsResults . fmap (second (pure :: a -> Either () a)))
                                       inSrc incDir excludes

unitsInfer inSrc excludes m debug incDir = do
    putStrLn $ "Inferring units for '" ++ inSrc ++ "'"
    uo <- optsToUnitOpts m debug incDir
    let rfun = LU.inferUnits uo
    doAnalysisReportWithModFiles rfun printUnitsResults inSrc incDir excludes

unitsCompile inSrc excludes m debug incDir outSrc = do
    putStrLn $ "Compiling units for '" ++ inSrc ++ "'"
    uo <- optsToUnitOpts m debug incDir
    let rfun = LU.compileUnits uo
    putStrLn =<< doCreateBinary rfun inSrc incDir excludes outSrc


unitsSynth inSrc excludes m debug incDir outSrc annType = do
    putStrLn $ "Synthesising units for '" ++ inSrc ++ "'"
    let marker = markerChar annType
    uo <- optsToUnitOpts m debug incDir
    let rfun = traverse (\fp ->
                           case runRefactoring (LU.synthesiseUnits uo marker) fp of
                             ((debug, Nothing), res) -> ([debug], res)
                             ((debug, Just ir), res) -> ([debug ++ show ir], res))
    report <- doRefactorWithModFiles rfun concat inSrc incDir excludes outSrc
    putStrLn report

unitsCriticals inSrc excludes m debug incDir = do
    putStrLn $ "Suggesting variables to annotate with unit specifications in '"
             ++ inSrc ++ "'"
    uo <- optsToUnitOpts m debug incDir
    let rfun = LU.inferCriticalVariables uo
    doAnalysisReportWithModFiles rfun (mapM_ (putStrLn . fst)) inSrc incDir excludes

{- Stencils feature -}
stencilsCheck inSrc excludes = do
   putStrLn $ "Checking stencil specs for '" ++ inSrc ++ "'"
   doAnalysisSummary Stencils.check inSrc excludes

stencilsInfer inSrc excludes useEval = do
   putStrLn $ "Inferring stencil specs for '" ++ inSrc ++ "'"
   let rfun = Stencils.infer useEval '='
   doAnalysisSummary rfun inSrc excludes

stencilsSynth inSrc excludes annType outSrc = do
   putStrLn $ "Synthesising stencil specs for '" ++ inSrc ++ "'"
   let rfun = second (fmap (pure :: a -> Either () a)) . Stencils.synth (markerChar annType)
   report <- doRefactor rfun id inSrc excludes outSrc
   putStrLn report

-- | Initialize Camfort for the given project.
camfortInitialize :: FilePath -> IO ()
camfortInitialize projectDir =
  createDirectoryIfMissing False (projectDir </> ".camfort")
