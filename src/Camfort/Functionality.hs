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
  , implicitNone
  , allocCheck
  -- ** Stencil Analysis
  , stencilsCheck
  , stencilsInfer
  , stencilsSynth
  -- ** Unit Analysis
  , unitsCriticals
  , unitsCheck
  , unitsDump
  , unitsInfer
  , unitsCompile
  , unitsSynth
  -- ** Invariants Analysis
  , invariantsCheck
  -- ** Refactorings
  , common
  , dead
  , equivalences
  , ddtRefactor
  , ddtInfer
  , ddtCheck
  , ddtSynth
  , ddtCompile
  -- ** Project Management
  , camfortInitialize
  ) where

import           Control.Arrow                                   (first, second)
import           Data.List                                       (intersperse)
import           Data.Maybe                                      (fromMaybe)
import           Data.Void                                       (Void)
import qualified Data.ByteString                                 as B
import qualified Data.ByteString.Lazy                            as LB
import           System.Directory                                (doesDirectoryExist, createDirectoryIfMissing,
                                                                  getCurrentDirectory)
import           System.FilePath                                 (takeDirectory,
                                                                  (</>), replaceExtension)

import           Text.PrettyPrint.GenericPretty                  (pp, pretty, Out)

import           Control.Lens
import           Control.Monad.Reader.Class
import           Control.Monad
import           Control.Monad.IO.Class

import qualified Language.Fortran.AST                            as F
import qualified Language.Fortran.Analysis.Renaming              as FA
import qualified Language.Fortran.Analysis                       as FA
import qualified Language.Fortran.Util.ModFile                   as FM
import           Language.Fortran.ParserMonad                    (FortranVersion(..))
import           Language.Fortran.PrettyPrint

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
import qualified Camfort.Specification.DerivedDataType           as DDT
import qualified Camfort.Specification.Stencils                  as Stencils
import           Camfort.Specification.Stencils.Analysis         (compileStencils)
import qualified Camfort.Specification.Units                     as LU
import           Camfort.Specification.Units.Analysis            (compileUnits)
import           Camfort.Specification.Units.Annotation          (unitInfo)
import           Camfort.Specification.Units.Analysis.Consistent (checkUnits)
import           Camfort.Specification.Units.Analysis.Criticals  (inferCriticalVariables)
import           Camfort.Specification.Units.Analysis.Infer      (InferenceResult(..), InferenceReport(..), inferUnits)
import           Camfort.Specification.Units.Monad               (runUnitAnalysis,
                                                                  unitOpts0)
import           Camfort.Specification.Units.ModFile             (dumpModFileCompiledUnits)
import           Camfort.Specification.Units.MonadTypes          (LiteralsOpt,
                                                                  UnitAnalysis,
                                                                  UnitEnv (..),
                                                                  UnitOpts (..))
import qualified Camfort.Specification.Hoare as Hoare
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
  { ceInputSources   :: FileOrDir
  , ceIncludeDir     :: Maybe FileOrDir
  , ceExcludeFiles   :: [Filename]
  , ceLogLevel       :: LogLevel
  , ceFortranVersion :: Maybe FortranVersion
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
  pfsTexts <- readParseSrcDir (ceFortranVersion env) modFiles (ceInputSources env) (ceExcludeFiles env)
  runner program (logOutputStd True) (ceLogLevel env) modFiles pfsTexts



--------------------------------------------------------------------------------
-- * Wrappers on all of the features

ast :: CamfortEnv -> IO ()
ast env = do
    incDir' <- maybe getCurrentDirectory pure (ceIncludeDir env)
    modFiles <- getModFiles incDir'
    xs <- readParseSrcDir (ceFortranVersion env) modFiles (ceInputSources env) (ceExcludeFiles env)
    print . fmap fst $ xs


countVarDecls :: CamfortEnv -> IO Int
countVarDecls =
  runFunctionality
  "Counting variable declarations in"
  (generalizePureAnalysis . countVariableDeclarations)
  (describePerFileAnalysis "count variable declarations")
  simpleCompiler ()

dead :: FileOrDir -> CamfortEnv -> IO Int
dead =
  runWithOutput
  "Eliminating dead code in"
  (fmap generalizePureAnalysis . perFileRefactoring $ deadCode False)
  (doRefactor "dead code elimination")
  simpleCompiler ()


common :: FileOrDir -> CamfortEnv -> IO Int
common outSrc =
  runWithOutput
  "Refactoring common blocks in"
  (generalizePureAnalysis . commonElimToModules (takeDirectory outSrc ++ "/"))
  (doRefactorAndCreate "common block refactoring")
  simpleCompiler ()
  outSrc


equivalences :: FileOrDir -> CamfortEnv -> IO Int
equivalences =
  runWithOutput
  "Refactoring equivalences blocks in"
  (fmap generalizePureAnalysis . perFileRefactoring $ refactorEquivalences)
  (doRefactor "equivalence block refactoring")
  simpleCompiler ()

implicitNone :: Bool -> CamfortEnv -> IO Int
implicitNone allPU =
  runFunctionality
  "Checking 'implicit none' completeness"
  (generalizePureAnalysis . (checkImplicitNone allPU))
  (describePerFileAnalysis "check 'implicit none'")
  simpleCompiler ()

allocCheck :: CamfortEnv -> IO Int
allocCheck =
  runFunctionality
  "Checking allocate / deallocate usage"
  (generalizePureAnalysis . checkAllocateStatements)
  (describePerFileAnalysis "check allocate / deallocate")
  simpleCompiler ()

ddtRefactor :: FileOrDir -> CamfortEnv -> IO Int
ddtRefactor =
  runWithOutput
  "Refactoring derived datatypes"
  (generalizePureAnalysis . DDT.refactor)
  (doRefactor "infer derived data types")
  simpleCompiler ()

ddtSynth :: AnnotationType -> FileOrDir -> CamfortEnv -> IO Int
ddtSynth annType =
  runWithOutput
  "Synthesising derived datatypes"
  (generalizePureAnalysis . DDT.synth (markerChar annType))
  (doRefactor "synth derived data types")
  simpleCompiler ()

ddtCheck :: CamfortEnv -> IO Int
ddtCheck =
  runFunctionality
  "Checking derived datatype annotations"
  (generalizePureAnalysis . DDT.check)
  (describePerFileAnalysis "check derived datatypes")
  simpleCompiler ()

ddtInfer :: CamfortEnv -> IO Int
ddtInfer =
  runFunctionality
  "Inferring derived datatypes"
  (generalizePureAnalysis . DDT.infer)
  (describePerFileAnalysis "infer derived datatypes")
  simpleCompiler ()

ddtCompile :: CamfortEnv -> IO Int
ddtCompile env = do
  let description = "Compiling derived datatypes for"
  putStrLn $ description ++ " '" ++ ceInputSources env ++ "'"
  incDir' <- maybe getCurrentDirectory pure (ceIncludeDir env)
  isDir <- doesDirectoryExist incDir'
  let incDir | isDir     = incDir'
             | otherwise = takeDirectory incDir'
  modFiles <- getModFiles incDir

  -- Run the gen mod file routine directly on the input source
  modFiles <- genModFiles (ceFortranVersion env) modFiles DDT.compile () (ceInputSources env) (ceExcludeFiles env)
  -- Write the mod files out
  forM_ modFiles $ \ modFile -> do
     let mfname = replaceExtension (FM.moduleFilename modFile) FM.modFileSuffix
     LB.writeFile mfname (FM.encodeModFile modFile)
  return 0

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

unitsDump :: LiteralsOpt -> CamfortEnv -> IO Int
unitsDump _ env = do
  let modFileName = ceInputSources env
  modData <- LB.readFile modFileName
  let eResult = FM.decodeModFile modData
  case eResult of
    Left msg -> do
      putStrLn $ modFileName ++ ": Error: " ++ show msg
      pure 1
    Right modFile -> do
      putStrLn $ modFileName ++ ": successfully parsed precompiled file."
      putStrLn . fromMaybe "unable to find units info" $ dumpModFileCompiledUnits modFile
      pure 0

unitsCheck :: LiteralsOpt -> CamfortEnv -> IO Int
unitsCheck =
  runUnitsFunctionality
  "Checking units for"
  (singlePfUnits checkUnits)
  (describePerFileAnalysis "unit checking")


unitsInfer :: Bool -> LiteralsOpt -> CamfortEnv -> IO Int
unitsInfer showAST =
  runUnitsFunctionality
  "Inferring units for"
  (singlePfUnits inferUnits)
  (if showAST
      then describePerFileAnalysisShowASTUnitInfo
      else describePerFileAnalysis "unit inference")

-- | Given an analysis program for a single file, run it over every input file
-- and collect the reports, then print those reports to standard output.
describePerFileAnalysisShowASTUnitInfo
  :: (MonadIO m, Describe w, Describe e)
  => AnalysisRunner e w m ProgramFile InferenceResult Int
describePerFileAnalysisShowASTUnitInfo program logOutput logLevel modFiles pfsTexts = do
  reports <- runPerFileAnalysis program logOutput logLevel modFiles pfsTexts
  flip mapM_ reports $ \ r -> do
    case r of
      AnalysisReport _ _ (ARSuccess (Inferred (InferenceReport pfUA eVars))) ->
        liftIO . pp . fmap (unitInfo . FA.prevAnnotation) . FA.rename $ pfUA
      _ -> pure ()
    putDescribeReport "unit inference" (Just logLevel) r
  return $ exitCodeOfSet reports

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

unitsCompile :: LiteralsOpt -> CamfortEnv -> IO Int
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
  modFiles <- genModFiles (ceFortranVersion env) modFiles compileUnits uo (ceInputSources env) (ceExcludeFiles env)
  -- Write the mod files out
  forM_ modFiles $ \modFile -> do
     let mfname = replaceExtension (FM.moduleFilename modFile) FM.modFileSuffix
     LB.writeFile mfname (FM.encodeModFile modFile)
  return 0

unitsSynth :: AnnotationType -> FileOrDir -> LiteralsOpt -> CamfortEnv -> IO Int
unitsSynth annType outSrc opts env =
  runUnitsFunctionality
  "Synthesising units for"
  (multiPfUnits $ LU.synthesiseUnits (markerChar annType))
  (doRefactor "unit synthesis" (ceInputSources env) outSrc)
  opts
  env


unitsCriticals :: LiteralsOpt -> CamfortEnv -> IO Int
unitsCriticals =
  runUnitsFunctionality
  "Suggesting variables to annotate with unit specifications in"
  (singlePfUnits inferCriticalVariables)
  (describePerFileAnalysis "unit critical variable analysis")


{- Stencils feature -}


stencilsCheck :: CamfortEnv -> IO Int
stencilsCheck =
  runFunctionality
  "Checking stencil specs for"
  (generalizePureAnalysis . Stencils.check)
  (describePerFileAnalysis "stencil checking")
  compileStencils ()


stencilsInfer :: Bool -> CamfortEnv -> IO Int
stencilsInfer useEval =
  runFunctionality
  "Inferring stencil specs for"
  (generalizePureAnalysis . Stencils.infer useEval '=')
  (describePerFileAnalysis "stencil inference")
  compileStencils ()


stencilsSynth :: AnnotationType -> FileOrDir -> CamfortEnv -> IO Int
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

invariantsCheck :: Hoare.PrimReprOption -> CamfortEnv -> IO Int
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
