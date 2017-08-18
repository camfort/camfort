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

{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Camfort.Functionality
  (
  -- * Datatypes
    AnnotationType(..)
  , CamfortEnv(..)
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

import           Control.Arrow                                   (first, second)
import           Data.List                                       (intersperse)
import           Data.Void                                       (Void)
import           System.Directory                                (createDirectoryIfMissing,
                                                                  getCurrentDirectory)
import           System.FilePath                                 (takeDirectory,
                                                                  (</>))

import           Control.Lens
import           Control.Monad.Reader.Class

import qualified Language.Fortran.AST                            as F

import           Camfort.Analysis
import           Camfort.Analysis.Annotations                    (Annotation)
import           Camfort.Analysis.Logger
import           Camfort.Analysis.ModFile                        (MFCompiler,
                                                                  genModFiles,
                                                                  readParseSrcDir,
                                                                  simpleCompiler)
import           Camfort.Analysis.Simple
import           Camfort.Input
import qualified Camfort.Specification.Stencils                  as Stencils
import           Camfort.Specification.Stencils.Analysis         (compileStencils)
import qualified Camfort.Specification.Units                     as LU
import           Camfort.Specification.Units.Analysis            (compileUnits)
import           Camfort.Specification.Units.Analysis.Consistent (checkUnits)
import           Camfort.Specification.Units.Analysis.Criticals  (inferCriticalVariables)
import           Camfort.Specification.Units.Analysis.Infer      (inferUnits)
import           Camfort.Specification.Units.Monad               (runUnitAnalysis,
                                                                  unitOpts0)
import           Camfort.Specification.Units.MonadTypes          (LiteralsOpt,
                                                                  UnitAnalysis,
                                                                  UnitEnv (..),
                                                                  UnitOpts (..))
import           Camfort.Transformation.CommonBlockElim
import           Camfort.Transformation.DeadCode
import           Camfort.Transformation.EquivalenceElim

import           Camfort.Helpers                                 (FileOrDir,
                                                                  Filename)

data AnnotationType = ATDefault | Doxygen | Ford


-- | Retrieve the marker character compatible with the given
-- type of annotation.
markerChar :: AnnotationType -> Char
markerChar Doxygen   = '<'
markerChar Ford      = '!'
markerChar ATDefault = '='

data CamfortEnv =
  CamfortEnv
  { ceInputSources :: FileOrDir
  , ceIncludeDir   :: Maybe FileOrDir
  , ceExcludeFiles :: [Filename]
  , ceLogLevel     :: LogLevel
  }

--------------------------------------------------------------------------------
-- *  Running Functionality

runWithOutput
  :: (Describe e, Describe w)
  => String
  -- ^ Functionality desription
  -> AnalysisProgram e w IO a b
  -- ^ Analysis program
  -> (FileOrDir -> FilePath -> AnalysisRunner e w IO a b r)
  -- ^ Analysis runner
  -> MFCompiler i IO
  -- ^ Mod file compiler
  -> i
  -- ^ Mod file input
  -> FilePath
  -> CamfortEnv
  -> IO r
runWithOutput description program runner mfCompiler mfInput outSrc env =
  let runner' = runner (ceInputSources env) outSrc
  in runFunctionality description program runner' mfCompiler mfInput env


runFunctionality
  :: (Describe e, Describe w)
  => String
  -- ^ Functionality desription
  -> AnalysisProgram e w IO a b
  -- ^ Analysis program
  -> AnalysisRunner e w IO a b r
  -- ^ Analysis runner
  -> MFCompiler i IO
  -- ^ Mod file compiler
  -> i
  -- ^ Mod file input
  -> CamfortEnv
  -> IO r
runFunctionality description program runner mfCompiler mfInput env = do
  putStrLn $ description ++ " '" ++ ceInputSources env ++ "'"
  incDir <- maybe getCurrentDirectory pure (ceIncludeDir env)
  modFiles <- genModFiles mfCompiler mfInput incDir (ceExcludeFiles env)
  pfsTexts <- readParseSrcDir modFiles (ceInputSources env) (ceExcludeFiles env)
  runner program logOutputStd (ceLogLevel env) modFiles pfsTexts


--------------------------------------------------------------------------------
-- * Wrappers on all of the features

ast :: CamfortEnv -> IO ()
ast env = do
    incDir' <- maybe getCurrentDirectory pure (ceIncludeDir env)
    modFiles <- genModFiles simpleCompiler () incDir' (ceExcludeFiles env)
    xs <- readParseSrcDir modFiles (ceInputSources env) (ceExcludeFiles env)
    print . fmap fst $ xs


countVarDecls :: CamfortEnv -> IO ()
countVarDecls =
  runFunctionality
  "Counting variable declarations in"
  (generalizePureAnalysisProgram countVariableDeclarations)
  describePerFileAnalysis
  simpleCompiler ()


perFileRefactoring
  :: (Monad m)
  => AnalysisProgram e w m ProgramFile ProgramFile
  -> AnalysisProgram e w m [ProgramFile] ((), [Either e ProgramFile])
perFileRefactoring program modfiles pfs = do
  pfs' <- mapM (program modfiles) pfs
  return ((), fmap pure pfs')


dead :: FileOrDir -> CamfortEnv -> IO ()
dead =
  runWithOutput
  "Eliminating dead code in"
  (generalizePureAnalysisProgram . perFileRefactoring . const $ deadCode False)
  doRefactor
  simpleCompiler ()


common :: FileOrDir -> CamfortEnv -> IO ()
common outSrc =
  runWithOutput
  "Refactoring common blocks in"
  (generalizePureAnalysisProgram $ \_ -> commonElimToModules (takeDirectory outSrc ++ "/"))
  doRefactorAndCreate
  simpleCompiler ()
  outSrc


equivalences :: FileOrDir -> CamfortEnv -> IO ()
equivalences =
  runWithOutput
  "Refactoring equivalences blocks in"
  (generalizePureAnalysisProgram . perFileRefactoring . const $ refactorEquivalences)
  doRefactor
  simpleCompiler ()

-- {- Units feature -}


-- TODO: Get rid of debug option and replace with log level

runUnitsFunctionality
  :: (Describe e, Describe w)
  => String
  -> (UnitOpts -> AnalysisProgram e w IO a b)
  -> AnalysisRunner e w IO a b r
  -> LiteralsOpt
  -> Bool
  -> CamfortEnv
  -> IO r
runUnitsFunctionality description unitsProgram runner opts debug =
  let uo = optsToUnitOpts opts debug
  in runFunctionality description (unitsProgram uo) runner compileUnits uo

optsToUnitOpts :: LiteralsOpt -> Bool -> UnitOpts
optsToUnitOpts m debug = o1
  where o1 = unitOpts0 { uoLiterals = m
                       , uoDebug = debug
                       }

singlePfUnits :: UnitAnalysis a -> UnitOpts -> AnalysisProgram () () IO ProgramFile a
singlePfUnits unitAnalysis opts modfiles pf =
  let ue = UnitEnv
        { unitOpts = opts
        , unitModfiles = modfiles
        , unitProgramFile = pf
        }
  in runUnitAnalysis ue unitAnalysis

multiPfUnits
  :: (Describe a)
  => UnitAnalysis (Either e (a, b))
  -> UnitOpts
  -> AnalysisProgram () () IO [ProgramFile] (Text, [Either e b])
multiPfUnits unitAnalysis opts modfiles pfs = do
  let ue pf = UnitEnv
        { unitOpts = opts
        , unitModfiles = modfiles
        , unitProgramFile = pf
        }

  results <- traverse (\pf -> runUnitAnalysis (ue pf) unitAnalysis) pfs
  let (rs, ps) = traverse (traverse (\(x, y) -> ([x], y))) results

      rs' = mconcat . intersperse "\n" . map describe $ rs

  return (rs', ps)

unitsCheck :: LiteralsOpt -> Bool -> CamfortEnv -> IO ()
unitsCheck =
  runUnitsFunctionality
  "Checking units for"
  (singlePfUnits checkUnits)
  describePerFileAnalysis


unitsInfer :: LiteralsOpt -> Bool -> CamfortEnv -> IO ()
unitsInfer =
  runUnitsFunctionality
  "Inferring units for"
  (singlePfUnits inferUnits)
  describePerFileAnalysis


unitsSynth :: AnnotationType -> FileOrDir -> LiteralsOpt -> Bool -> CamfortEnv -> IO ()
unitsSynth annType outSrc opts debug env =
  runUnitsFunctionality
  "Synthesising units for"
  (multiPfUnits $ LU.synthesiseUnits (markerChar annType))
  (doRefactor (ceInputSources env) outSrc)
  opts
  debug
  env


unitsCriticals :: LiteralsOpt -> Bool -> CamfortEnv -> IO ()
unitsCriticals =
  runUnitsFunctionality
  "Suggesting variables to annotate with unit specifications in"
  (singlePfUnits inferCriticalVariables)
  describePerFileAnalysis


{- Stencils feature -}


stencilsCheck :: CamfortEnv -> IO ()
stencilsCheck =
  runFunctionality
  "Checking stencil specs for"
  (generalizePureAnalysisProgram Stencils.check)
  describePerFileAnalysis
  compileStencils ()


stencilsInfer :: Bool -> CamfortEnv -> IO ()
stencilsInfer useEval =
  runFunctionality
  "Inferring stencil specs for"
  (generalizePureAnalysisProgram (Stencils.infer useEval '='))
  describePerFileAnalysis
  compileStencils ()


stencilsSynth :: AnnotationType -> FileOrDir -> CamfortEnv -> IO ()
stencilsSynth annType =
  let
    program :: AnalysisProgram () () IO [ProgramFile] ((), [Either () ProgramFile])
    program pfs modfiles = generalizePureAnalysis $ do
      pfs' <- Stencils.synth (markerChar annType) pfs modfiles
      return ((), map Right pfs')

  in runWithOutput
     "Synthesising stencil specs for"
     program
     doRefactor
     compileStencils ()


-- | Initialize Camfort for the given project.
camfortInitialize :: FilePath -> IO ()
camfortInitialize projectDir =
  createDirectoryIfMissing False (projectDir </> ".camfort")
