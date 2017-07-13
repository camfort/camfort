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
  ) where

import System.FilePath
import Control.Monad

import Data.Binary

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
    print (map (\(_, _, p) -> p) xs)

countVarDecls inSrc excludes = do
    putStrLn $ "Counting variable declarations in '" ++ inSrc ++ "'"
    doAnalysisSummary countVariableDeclarations inSrc excludes

dead inSrc excludes outSrc = do
    putStrLn $ "Eliminating dead code in '" ++ inSrc ++ "'"
    report <- doRefactor (mapM (deadCode False)) inSrc excludes outSrc
    putStrLn report

common inSrc excludes outSrc = do
    putStrLn $ "Refactoring common blocks in '" ++ inSrc ++ "'"
    isDir <- isDirectory inSrc
    let rfun = commonElimToModules (takeDirectory outSrc ++ "/")
    report <- doRefactorAndCreate rfun inSrc excludes outSrc
    putStrLn report

equivalences inSrc excludes outSrc = do
    putStrLn $ "Refactoring equivalences blocks in '" ++ inSrc ++ "'"
    report <- doRefactor (mapM refactorEquivalences) inSrc excludes outSrc
    putStrLn report

{- Units feature -}
optsToUnitOpts :: LiteralsOpt -> Bool -> Maybe String -> IO UnitOpts
optsToUnitOpts m debug = maybe (pure o1) (\d -> do
  -- Figure out the camfort mod files and parse them.
  modFileNames <- filter isModFile `fmap` rGetDirContents' d
  assocList <- forM modFileNames $ \ modFileName -> do
    eResult <- decodeFileOrFail (d ++ "/" ++ modFileName) -- FIXME, directory manipulation
    case eResult of
      Left (offset, msg) -> do
        putStrLn $ modFileName ++ ": Error at offset " ++ show offset ++ ": " ++ msg
        return (modFileName, emptyModFile)
      Right modFile -> do
        putStrLn $ modFileName ++ ": successfully parsed precompiled file."
        return (modFileName, modFile)
  return $ o1 { uoModFiles = M.fromList assocList })
  -- return $ o1 { uoModFiles = M.fromList assocList `M.union` uoModFiles o })
  where o1 = unitOpts0 { uoLiterals = m
                       , uoDebug = debug
                       , uoModFiles = M.empty }


getModFiles :: Maybe String -> IO ModFiles
getModFiles = maybe (pure emptyModFiles) (\d -> do
  -- Figure out the camfort mod files and parse them.
  modFileNames <- filter isModFile `fmap` rGetDirContents' d
  addedModFiles <- forM modFileNames $ \ modFileName -> do
    eResult <- decodeFileOrFail (d ++ "/" ++ modFileName) -- FIXME, directory manipulation
    case eResult of
      Left (offset, msg) -> do
        putStrLn $ modFileName ++ ": Error at offset " ++ show offset ++ ": " ++ msg
        return emptyModFile
      Right modFile -> do
        putStrLn $ modFileName ++ ": successfully parsed precompiled file."
        return modFile
  return addedModFiles)

isModFile = (== modFileSuffix) . takeExtension

unitsCheck inSrc excludes m debug incDir = do
    putStrLn $ "Checking units for '" ++ inSrc ++ "'"
    uo <- optsToUnitOpts m debug incDir
    let rfun = concatMap (LU.checkUnits uo)
    doAnalysisReportWithModFiles rfun putStrLn inSrc excludes =<< getModFiles incDir

unitsInfer inSrc excludes m debug incDir = do
    putStrLn $ "Inferring units for '" ++ inSrc ++ "'"
    uo <- optsToUnitOpts m debug incDir
    let rfun = concatMap (LU.inferUnits uo)
    doAnalysisReportWithModFiles rfun putStrLn inSrc excludes =<< getModFiles incDir

unitsCompile inSrc excludes m debug incDir outSrc = do
    putStrLn $ "Compiling units for '" ++ inSrc ++ "'"
    uo <- optsToUnitOpts m debug incDir
    let rfun = LU.compileUnits uo
    putStrLn =<< doCreateBinary rfun inSrc excludes outSrc =<< getModFiles incDir


unitsSynth inSrc excludes m debug incDir outSrc annType = do
    putStrLn $ "Synthesising units for '" ++ inSrc ++ "'"
    let marker = markerChar annType
    uo <- optsToUnitOpts m debug incDir
    let rfun =
          mapM (LU.synthesiseUnits uo marker)
    report <- doRefactorWithModFiles rfun inSrc excludes outSrc =<< getModFiles incDir
    putStrLn report

unitsCriticals inSrc excludes m debug incDir = do
    putStrLn $ "Suggesting variables to annotate with unit specifications in '"
             ++ inSrc ++ "'"
    uo <- optsToUnitOpts m debug incDir
    let rfun = mapM (LU.inferCriticalVariables uo)
    doAnalysisReportWithModFiles rfun (putStrLn . fst) inSrc excludes =<< getModFiles incDir

{- Stencils feature -}
stencilsCheck inSrc excludes = do
   putStrLn $ "Checking stencil specs for '" ++ inSrc ++ "'"
   let rfun = \f p -> (Stencils.check f p, p)
   doAnalysisSummary rfun inSrc excludes

stencilsInfer inSrc excludes inferMode = do
   putStrLn $ "Inferring stencil specs for '" ++ inSrc ++ "'"
   let rfun = Stencils.infer inferMode '='
   doAnalysisSummary rfun inSrc excludes

stencilsSynth inSrc excludes inferMode annType outSrc = do
   putStrLn $ "Synthesising stencil specs for '" ++ inSrc ++ "'"
   let rfun = Stencils.synth inferMode (markerChar annType)
   report <- doRefactor rfun inSrc excludes outSrc
   putStrLn report
