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
  , unitsCompile
  , unitsSynth
  -- ** Invariants Analysis
  , invariantsCheck
  -- ** Refactorings
  , common
  , dead
  , equivalences
  , arrayIndexSwap
  -- ** Project Management
  , camfortInitialize
  ) where

import           Control.Arrow                                   (first, second)
import           Data.List                                       (intersperse)
import           Data.Void                                       (Void)
import qualified Data.ByteString as B
import           System.Directory                                (doesDirectoryExist, createDirectoryIfMissing,
                                                                  getCurrentDirectory)
import           System.FilePath                                 (takeDirectory,
                                                                  (</>), replaceExtension)

import           Control.Lens
import           Control.Monad.Reader.Class
import           Control.Monad (forM_)

import qualified Language.Fortran.AST                            as F
import qualified Language.Fortran.Util.ModFile                   as FM

import           Camfort.Analysis
import           Camfort.Analysis.Annotations                    (Annotation)
import           Camfort.Analysis.Logger
import           Camfort.Analysis.ModFile                        (MFCompiler,
                                                                  getModFiles,
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
import qualified Camfort.Specification.Hoare as Hoare
import           Camfort.Transformation.CommonBlockElim
import           Camfort.Transformation.DeadCode
import           Camfort.Transformation.EquivalenceElim
import           Camfort.Transformation.ArrayIndexSwap

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
  incDir' <- maybe getCurrentDirectory pure (ceIncludeDir env)
  isDir <- doesDirectoryExist incDir'
  let incDir | isDir     = incDir'
             | otherwise = takeDirectory incDir'
  -- Previously...
--modFiles <- genModFiles mfCompiler mfInput incDir (ceExcludeFiles env)
  -- ...instead for now, just get the mod files

  modFiles <- getModFiles incDir
  pfsTexts <- readParseSrcDir modFiles (ceInputSources env) (ceExcludeFiles env)
  runner program (logOutputStd True) (ceLogLevel env) modFiles pfsTexts



--------------------------------------------------------------------------------
-- * Wrappers on all of the features

ast :: CamfortEnv -> IO ()
ast env = do
    incDir' <- maybe getCurrentDirectory pure (ceIncludeDir env)
    modFiles <- getModFiles incDir'
    xs <- readParseSrcDir modFiles (ceInputSources env) (ceExcludeFiles env)
    print . fmap fst $ xs


countVarDecls :: CamfortEnv -> IO ()
countVarDecls =
  runFunctionality
  "Counting variable declarations in"
  (generalizePureAnalysis . countVariableDeclarations)
  (describePerFileAnalysis "count variable declarations")
  simpleCompiler ()


dead :: FileOrDir -> CamfortEnv -> IO ()
dead =
  runWithOutput
  "Eliminating dead code in"
  (fmap generalizePureAnalysis . perFileRefactoring $ deadCode False)
  (doRefactor "dead code elimination")
  simpleCompiler ()


common :: FileOrDir -> CamfortEnv -> IO ()
common outSrc =
  runWithOutput
  "Refactoring common blocks in"
  (generalizePureAnalysis . commonElimToModules (takeDirectory outSrc ++ "/"))
  (doRefactorAndCreate "common block refactoring")
  simpleCompiler ()
  outSrc


equivalences :: FileOrDir -> CamfortEnv -> IO ()
equivalences =
  runWithOutput
  "Refactoring equivalences blocks in"
  (fmap generalizePureAnalysis . perFileRefactoring $ refactorEquivalences)
  (doRefactor "equivalence block refactoring")
  simpleCompiler ()


arrayIndexSwap :: Int -> Int -> F.Name -> FileOrDir -> CamfortEnv -> IO ()
arrayIndexSwap i j name =
  runWithOutput
  ("For top-level array " ++ name ++  " swapping array indices " ++
    show i ++ " for " ++ show j)
  (fmap generalizePureAnalysis . perFileRefactoring $ swapIndices i j name)
  (doRefactor "array index swapping")
  simpleCompiler ()

{- Units feature -}

runUnitsFunctionality
  :: (Describe e, Describe w)
  => String
  -> (UnitOpts -> AnalysisProgram e w IO a b)
  -> AnalysisRunner e w IO a b r
  -> LiteralsOpt
  -> CamfortEnv
  -> IO r
runUnitsFunctionality description unitsProgram runner opts =
  let uo = optsToUnitOpts opts
  in runFunctionality description (unitsProgram uo) runner compileUnits uo

optsToUnitOpts :: LiteralsOpt -> UnitOpts
optsToUnitOpts m = o1
  where o1 = unitOpts0 { uoLiterals = m
                       }

singlePfUnits
  :: UnitAnalysis a -> UnitOpts
  -> AnalysisProgram () () IO ProgramFile a
singlePfUnits unitAnalysis opts pf =
  let ue = UnitEnv
        { unitOpts = opts
        , unitProgramFile = pf
        }
  in runUnitAnalysis ue unitAnalysis


-- slight hack to make doRefactorAndCreate happy
singlePfUnits'
  :: UnitAnalysis a -> UnitOpts
  -> AnalysisProgram () () IO [ProgramFile] a
singlePfUnits' unitAnalysis opts (pf:_) =
  let ue = UnitEnv
        { unitOpts = opts
        , unitProgramFile = pf
        }
  in runUnitAnalysis ue unitAnalysis

multiPfUnits
  :: (Describe a)
  => UnitAnalysis (Either e (a, b))
  -> UnitOpts
  -> AnalysisProgram () () IO [ProgramFile] (Text, [Either e b])
multiPfUnits unitAnalysis opts pfs = do
  let ue pf = UnitEnv
        { unitOpts = opts
        , unitProgramFile = pf
        }

  results <- traverse (\pf -> runUnitAnalysis (ue pf) unitAnalysis) pfs
  let (rs, ps) = traverse (traverse (\(x, y) -> ([x], y))) results

      rs' = mconcat . intersperse "\n" . map describe $ rs

  return (rs', ps)

unitsCheck :: LiteralsOpt -> CamfortEnv -> IO ()
unitsCheck =
  runUnitsFunctionality
  "Checking units for"
  (singlePfUnits checkUnits)
  (describePerFileAnalysis "unit checking")


unitsInfer :: LiteralsOpt -> CamfortEnv -> IO ()
unitsInfer =
  runUnitsFunctionality
  "Inferring units for"
  (singlePfUnits inferUnits)
  (describePerFileAnalysis "unit inference")

{-  TODO: remove if not needed
unitsCompile :: FileOrDir -> LiteralsOpt -> CamfortEnv -> IO ()
unitsCompile outSrc opts env =
  runUnitsFunctionality
  "Compiling units for"
  (singlePfUnits inferAndCompileUnits)
  (compilePerFile "unit compilation" (ceInputSources env) outSrc)
  opts
  env
-}
  -- Previously...
--modFiles <- genModFiles mfCompiler mfInput incDir (ceExcludeFiles env)
  -- ...instead for now, just get the mod files

unitsCompile :: LiteralsOpt -> CamfortEnv -> IO ()
unitsCompile opts env = do
  let uo = optsToUnitOpts opts
  let description = "Compiling units for"
  putStrLn $ description ++ " '" ++ ceInputSources env ++ "'"
  incDir' <- maybe getCurrentDirectory pure (ceIncludeDir env)
  isDir <- doesDirectoryExist incDir'
  let incDir | isDir     = incDir'
             | otherwise = takeDirectory incDir'
  modFiles <- getModFiles incDir

  -- Run the gen mod file routine directly on the input source
  modFiles <- genModFiles modFiles compileUnits uo (ceInputSources env) (ceExcludeFiles env)
  -- Write the mod files out
  forM_ modFiles $ \modFile -> do
     let mfname = replaceExtension (FM.moduleFilename modFile) FM.modFileSuffix
     B.writeFile mfname (FM.encodeModFile modFile)

unitsSynth :: AnnotationType -> FileOrDir -> LiteralsOpt -> CamfortEnv -> IO ()
unitsSynth annType outSrc opts env =
  runUnitsFunctionality
  "Synthesising units for"
  (multiPfUnits $ LU.synthesiseUnits (markerChar annType))
  (doRefactor "unit synthesis" (ceInputSources env) outSrc)
  opts
  env


unitsCriticals :: LiteralsOpt -> CamfortEnv -> IO ()
unitsCriticals =
  runUnitsFunctionality
  "Suggesting variables to annotate with unit specifications in"
  (singlePfUnits inferCriticalVariables)
  (describePerFileAnalysis "unit critical variable analysis")


{- Stencils feature -}


stencilsCheck :: CamfortEnv -> IO ()
stencilsCheck =
  runFunctionality
  "Checking stencil specs for"
  (generalizePureAnalysis . Stencils.check)
  (describePerFileAnalysis "stencil checking")
  compileStencils ()


stencilsInfer :: Bool -> CamfortEnv -> IO ()
stencilsInfer useEval =
  runFunctionality
  "Inferring stencil specs for"
  (generalizePureAnalysis . Stencils.infer useEval '=')
  (describePerFileAnalysis "stencil inference")
  compileStencils ()


stencilsSynth :: AnnotationType -> FileOrDir -> CamfortEnv -> IO ()
stencilsSynth annType =
  let
    program :: AnalysisProgram () () IO [ProgramFile] ((), [Either () ProgramFile])
    program pfs = generalizePureAnalysis $ do
      pfs' <- Stencils.synth (markerChar annType) pfs
      return ((), map Right pfs')

  in runWithOutput
     "Synthesising stencil specs for"
     program
     (doRefactor "stencil synthesis")
     compileStencils ()

{- Invariants Feature-}

invariantsCheck :: Hoare.PrimReprOption -> CamfortEnv -> IO ()
invariantsCheck pro =
  runFunctionality
  "Checking invariants in"
  (Hoare.check pro)
  (describePerFileAnalysis "invariant checking")
  simpleCompiler ()


-- | Initialize Camfort for the given project.
camfortInitialize :: FilePath -> IO ()
camfortInitialize projectDir =
  createDirectoryIfMissing False (projectDir </> ".camfort")

