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

import System.FilePath
import Control.Monad

import Data.Generics.Uniplate.Operations
import Data.Data
import Data.Binary
import Data.Text (pack, unpack, split)

import Camfort.Analysis.Simple
import Camfort.Transformation.DataTypeIntroduction
import Camfort.Transformation.DeadCode
import Camfort.Transformation.CommonBlockElim
import Camfort.Transformation.EquivalenceElim

import qualified Camfort.Specification.Units as LU
import Camfort.Specification.Units.Monad

import Camfort.Helpers
import Camfort.Input

import qualified Language.Fortran.Parser.Any as FP
import Language.Fortran.Util.ModFile
import qualified Camfort.Specification.Stencils as Stencils
import qualified Data.Map.Strict as M

-- CamFort optional flags
data Flag = Version
         | Input String
         | Output String
         | Excludes String
         | IncludeDir String
         | Literals LiteralsOpt
         | StencilInferMode Stencils.InferMode
         | Doxygen
         | Ford
         | FVersion String
         | RefactorInPlace
         | Debug deriving (Data, Show, Eq)

type Options = [Flag]

-- Extract excluces information from options
instance Default String where
    defaultValue = ""
getExcludes :: Options -> String
getExcludes opts = head ([ e | Excludes e <- universeBi opts ] ++ [""])

-- Separates the comma-separated filenames
getExcludedFiles :: Options -> [String]
getExcludedFiles = map unpack . split (==',') . pack . getExcludes

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
    let rfun = commonElimToModules (takeDirectory outSrc ++ "/")
    report <- doRefactorAndCreate rfun inSrc excludes outSrc
    putStrLn report

equivalences inSrc excludes outSrc _ = do
    putStrLn $ "Refactoring equivalences blocks in '" ++ inSrc ++ "'"
    report <- doRefactor (mapM refactorEquivalences) inSrc excludes outSrc
    putStrLn report

datatypes inSrc excludes outSrc _ = do
    putStrLn $ "Introducing derived data types in '" ++ inSrc ++ "'"
    report <- doRefactor dataTypeIntro inSrc excludes outSrc
    putStrLn report

{- Units feature -}
optsToUnitOpts :: [Flag] -> IO UnitOpts
optsToUnitOpts = foldM (\ o f -> do
  case f of
    Literals m   -> return $ o { uoLiterals    = m }
    Debug        -> return $ o { uoDebug       = True }
    IncludeDir d -> do
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
      return $ o { uoModFiles = M.fromList assocList `M.union` uoModFiles o }
    _            -> return o
    ) unitOpts0

getModFiles :: [Flag] -> IO ModFiles
getModFiles = foldM (\ modFiles f -> do
  case f of
    IncludeDir d -> do
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
      return $ addedModFiles ++ modFiles
    _            -> return modFiles
    ) emptyModFiles

isModFile = (== modFileSuffix) . fileExt

unitsCheck inSrc excludes _ opt = do
    putStrLn $ "Checking units for '" ++ inSrc ++ "'"
    uo <- optsToUnitOpts opt
    let rfun = concatMap (LU.checkUnits uo)
    doAnalysisReportWithModFiles rfun putStrLn inSrc excludes =<< getModFiles opt

unitsInfer inSrc excludes _ opt = do
    putStrLn $ "Inferring units for '" ++ inSrc ++ "'"
    uo <- optsToUnitOpts opt
    let rfun = concatMap (LU.inferUnits uo)
    doAnalysisReportWithModFiles rfun putStrLn inSrc excludes =<< getModFiles opt

unitsCompile inSrc excludes outSrc opt = do
    putStrLn $ "Compiling units for '" ++ inSrc ++ "'"
    uo <- optsToUnitOpts opt
    let rfun = LU.compileUnits uo
    putStrLn =<< doCreateBinary rfun inSrc excludes outSrc =<< getModFiles opt

unitsSynth inSrc excludes outSrc opt = do
    putStrLn $ "Synthesising units for '" ++ inSrc ++ "'"
    let marker
         | Doxygen `elem` opt = '<'
         | Ford `elem` opt = '!'
         | otherwise = '='
    uo <- optsToUnitOpts opt
    let rfun =
          mapM (LU.synthesiseUnits uo marker)
    report <- doRefactorWithModFiles rfun inSrc excludes outSrc =<< getModFiles opt
    putStrLn report

unitsCriticals inSrc excludes _ opt = do
    putStrLn $ "Suggesting variables to annotate with unit specifications in '"
             ++ inSrc ++ "'"
    uo <- optsToUnitOpts opt
    let rfun = mapM (LU.inferCriticalVariables uo)
    doAnalysisReportWithModFiles rfun (putStrLn . fst) inSrc excludes =<< getModFiles opt

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
