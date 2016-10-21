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

module Camfort.Functionality where

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO

import Data.Monoid
import Data.Generics.Uniplate.Operations
import Data.Data
import Data.List (foldl', intercalate)
import qualified Debug.Trace as D

import Camfort.Analysis.Annotations
import Camfort.Analysis.Simple
import Camfort.Transformation.DeadCode
import Camfort.Transformation.CommonBlockElim
import Camfort.Transformation.EquivalenceElim

import qualified Camfort.Specification.Units as LU
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.Monad

import Camfort.Helpers.Syntax
import Camfort.Helpers
import Camfort.Output
import Camfort.Input

import qualified Language.Fortran.Parser.Any as FP
import qualified Language.Fortran.AST as F
import Language.Fortran.Analysis.Renaming
  (renameAndStrip, analyseRenames, unrename, NameMap)
import Language.Fortran.Analysis(initAnalysis)
import qualified Camfort.Specification.Stencils as Stencils

-- CamFort optional flags
data Flag = Version
         | Input String
         | Output String
         | Excludes String
         | Literals LiteralsOpt
         | StencilInferMode Stencils.InferMode
         | Doxygen
         | Ford
         | Debug deriving (Data, Show, Eq)

type Options = [Flag]

-- Extract excluces information from options
instance Default String where
    defaultValue = ""
getExcludes :: Options -> String
getExcludes = getOption

-- * Wrappers on all of the features
ast d excludes _ _ = do
    xs <- readParseSrcDir d excludes
    print (map (\(_, _, p) -> p) xs)

countVarDecls inSrc excludes _ _ = do
    putStrLn $ "Counting variable declarations in '" ++ inSrc ++ "'"
    doAnalysisSummary countVariableDeclarations inSrc excludes Nothing

dead inSrc excludes outSrc _ = do
    putStrLn $ "Eliminating dead code in '" ++ inSrc ++ "'"
    report <- doRefactor (mapM (deadCode False)) inSrc excludes outSrc
    putStrLn report

common inSrc excludes outSrc _ = do
    putStrLn $ "Refactoring common blocks in '" ++ inSrc ++ "'"
    isDir <- isDirectory inSrc
    let dir = if isDir then inSrc ++ "/" else ""
    let rfun = commonElimToModules (takeDirectory outSrc ++ "/")
    report <- doRefactorAndCreate rfun inSrc excludes outSrc
    putStrLn report

equivalences inSrc excludes outSrc _ = do
    putStrLn $ "Refactoring equivalences blocks in '" ++ inSrc ++ "'"
    report <- doRefactor (mapM refactorEquivalences) inSrc excludes outSrc
    putStrLn report

{- Units feature -}
optsToUnitOpts :: [Flag] -> UnitOpts
optsToUnitOpts = foldl' (\ o f -> case f of Literals m -> o { uoLiterals = m }
                                            Debug -> o { uoDebug = True }
                                            _     -> o) unitOpts0

unitsCheck inSrc excludes outSrc opt = do
    putStrLn $ "Checking units for '" ++ inSrc ++ "'"
    let rfun = concatMap (LU.checkUnits (optsToUnitOpts opt))
    doAnalysisReport rfun putStrLn inSrc excludes

unitsInfer inSrc excludes outSrc opt = do
    putStrLn $ "Inferring units for '" ++ inSrc ++ "'"
    let rfun = concatMap (LU.inferUnits (optsToUnitOpts opt))
    doAnalysisReport rfun putStrLn inSrc excludes

unitsCompile inSrc excludes outSrc opt = do
    putStrLn $ "Compiling units for '" ++ inSrc ++ "'"
    let rfun = LU.compileUnits (optsToUnitOpts opt)
    putStrLn =<< doCreateBinary rfun inSrc excludes outSrc

unitsSynth inSrc excludes outSrc opt = do
    putStrLn $ "Synthesising units for '" ++ inSrc ++ "'"
    let marker
         | Doxygen `elem` opt = '<'
         | Ford `elem` opt = '!'
         | otherwise = '='
    let rfun =
          mapM (LU.synthesiseUnits (optsToUnitOpts opt) marker)
    report <- doRefactor rfun inSrc excludes outSrc
    putStrLn report

unitsCriticals inSrc excludes outSrc opt = do
    putStrLn $ "Suggesting variables to annotate with unit specifications in '"
             ++ inSrc ++ "'"
    let rfun = mapM (LU.inferCriticalVariables (optsToUnitOpts opt))
    doAnalysisReport rfun (putStrLn . fst) inSrc excludes

{- Stencils feature -}
stencilsCheck inSrc excludes _ _ = do
   putStrLn $ "Checking stencil specs for '" ++ inSrc ++ "'"
   let rfun = \f p -> (Stencils.check f p, p)
   doAnalysisSummary rfun inSrc excludes Nothing

stencilsInfer inSrc excludes outSrc opt = do
   putStrLn $ "Infering stencil specs for '" ++ inSrc ++ "'"
   let rfun = Stencils.infer (getOption opt) '='
   doAnalysisSummary rfun inSrc excludes (Just outSrc)

stencilsSynth inSrc excludes outSrc opt = do
   putStrLn $ "Synthesising stencil specs for '" ++ inSrc ++ "'"
   let marker
        | Doxygen `elem` opt = '<'
        | Ford `elem` opt = '!'
        | otherwise = '='
   let rfun = Stencils.synth (getOption opt) marker
   report <- doRefactor rfun inSrc excludes outSrc
   putStrLn report
