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
  , fpCheck
  , useCheck
  , arrayCheck
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

import           Camfort.Analysis
import           Camfort.Analysis.Logger
import           Camfort.Analysis.ModFile (readParseSrcFile,  MFCompiler, getModFiles, genModFiles
                                          , readParseSrcDir, readParseSrcDirP, simpleCompiler)
import           Camfort.Analysis.Simple
import           Camfort.Helpers (FileOrDir, Filename)
import           Camfort.Input
import qualified Camfort.Specification.DerivedDataType as DDT
import qualified Camfort.Specification.Hoare as Hoare
import qualified Camfort.Specification.Stencils as Stencils
import           Camfort.Specification.Stencils.Analysis (compileStencils)
import qualified Camfort.Specification.Units as LU
import           Camfort.Specification.Units.Analysis (compileUnits)
import           Camfort.Specification.Units.Analysis.Consistent (checkUnits)
import           Camfort.Specification.Units.Analysis.Criticals (inferCriticalVariables)
import           Camfort.Specification.Units.Analysis.Infer (InferenceResult(..), InferenceReport(..), inferUnits)
import           Camfort.Specification.Units.Annotation (unitInfo)
import           Camfort.Specification.Units.ModFile (dumpModFileCompiledUnits)
import           Camfort.Specification.Units.Monad (runUnitAnalysis, unitOpts0)
import           Camfort.Specification.Units.MonadTypes (LiteralsOpt, UnitAnalysis, UnitEnv (..), UnitOpts (..))
import           Camfort.Transformation.CommonBlockElim
import           Camfort.Transformation.DeadCode
import           Camfort.Transformation.EquivalenceElim
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LB
import           Data.Char (toLower)
import           Data.List (intersperse)
import           Data.Maybe (fromMaybe)
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.ModGraph as FM
import qualified Language.Fortran.Analysis.Renaming as FA
import           Language.Fortran.Version (FortranVersion(..))
import qualified Language.Fortran.Util.ModFile as FM
import           Pipes
import qualified Pipes.Prelude as P
import           Prelude hiding (mod)
import           System.Directory
import           System.Directory (doesDirectoryExist, createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath (takeDirectory, (</>), takeExtension, replaceExtension)
import           System.IO
import           Text.PrettyPrint.GenericPretty (pp)

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
  , ceSourceSnippets :: Bool
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
runFunctionality description program runner _ _ env = do
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
  runner program (logOutputStd True) (ceLogLevel env) (ceSourceSnippets env) modFiles pfsTexts

runFunctionalityP
  :: (Describe e, Describe w, ExitCodeOfReport b)
  => String
  -- ^ Functionality desription
  -> AnalysisProgram e w IO a b
  -- ^ Analysis program
  -> AnalysisRunnerP e w IO a b (AnalysisReport e w b)
  -- ^ Analysis runner
  -> MFCompiler i IO
  -- ^ Mod file compiler
  -> i
  -- ^ Mod file input
  -> CamfortEnv
  -> IO Int
runFunctionalityP description program runner _ _ env = do
  putStrLn $ description ++ " '" ++ ceInputSources env ++ "'"
  incDir' <- maybe getCurrentDirectory pure (ceIncludeDir env)
  isDir <- doesDirectoryExist incDir'
  let incDir | isDir     = incDir'
             | otherwise = takeDirectory incDir'
  -- Previously...
--modFiles <- genModFiles mfCompiler mfInput incDir (ceExcludeFiles env)
  -- ...instead for now, just get the mod files

  modFiles <- getModFiles incDir

  reports <- P.toListM $
    readParseSrcDirP (ceFortranVersion env) modFiles (ceInputSources env) (ceExcludeFiles env) >->
    runner program (logOutputStd True) (ceLogLevel env) (ceSourceSnippets env) modFiles
  return . exitCodeOfSet $ reports

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
  runFunctionalityP
  "Counting variable declarations in"
  (generalizePureAnalysis . countVariableDeclarations)
  (describePerFileAnalysisP "count variable declarations")
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
  runFunctionalityP
  "Checking 'implicit none' completeness"
  (generalizePureAnalysis . (checkImplicitNone allPU))
  (describePerFileAnalysisP "check 'implicit none'")
  simpleCompiler ()

allocCheck :: CamfortEnv -> IO Int
allocCheck =
  runFunctionalityP
  "Checking allocate / deallocate usage"
  (generalizePureAnalysis . checkAllocateStatements)
  (describePerFileAnalysisP "check allocate / deallocate")
  simpleCompiler ()

fpCheck :: CamfortEnv -> IO Int
fpCheck =
  runFunctionalityP
  "Checking usage of floating point"
  (generalizePureAnalysis . checkFloatingPointUse)
  (describePerFileAnalysisP "check floating point")
  simpleCompiler ()

useCheck :: CamfortEnv -> IO Int
useCheck =
  runFunctionalityP
  "Checking usage of USE statements"
  (generalizePureAnalysis . checkModuleUse)
  (describePerFileAnalysisP "check module USE")
  simpleCompiler ()

arrayCheck :: CamfortEnv -> IO Int
arrayCheck =
  runFunctionalityP
  "Checking array usage"
  (generalizePureAnalysis . checkArrayUse)
  (describePerFileAnalysisP "check array usage")
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
  runFunctionalityP
  "Checking derived datatype annotations"
  (generalizePureAnalysis . DDT.check)
  (describePerFileAnalysisP "check derived datatypes")
  simpleCompiler ()

ddtInfer :: CamfortEnv -> IO Int
ddtInfer =
  runFunctionalityP
  "Inferring derived datatypes"
  (generalizePureAnalysis . DDT.infer)
  (describePerFileAnalysisP "infer derived datatypes")
  simpleCompiler ()

ddtCompile :: CamfortEnv -> IO Int
ddtCompile env = do
  let description = "Compiling derived datatypes for"
  putStrLn $ description ++ " '" ++ ceInputSources env ++ "'"
  incDir' <- maybe getCurrentDirectory pure (ceIncludeDir env)
  isDir <- doesDirectoryExist incDir'
  let incDir | isDir     = incDir'
             | otherwise = takeDirectory incDir'
  modFileNames <- getModFiles incDir

  -- Run the gen mod file routine directly on the input source
  modFiles <- genModFiles (ceFortranVersion env) modFileNames DDT.compile () (ceInputSources env) (ceExcludeFiles env)
  -- Write the mod files out
  forM_ modFiles $ \ modFile -> do
     let mfname = replaceExtension (FM.moduleFilename modFile) FM.modFileSuffix
     LB.writeFile mfname (FM.encodeModFile [modFile])
  return 0

{- Units feature -}

runUnitsFunctionalityP
  :: (Describe e, Describe w, ExitCodeOfReport b)
  => String
  -> (UnitOpts -> AnalysisProgram e w IO a b)
  -> AnalysisRunnerP e w IO a b (AnalysisReport e w b)
  -> LiteralsOpt
  -> Bool
  -> CamfortEnv
  -> IO Int
runUnitsFunctionalityP description unitsProgram runner opts uninits =
  let uo = optsToUnitOpts opts uninits
  in runFunctionalityP description (unitsProgram uo) runner compileUnits uo

optsToUnitOpts :: LiteralsOpt -> Bool -> UnitOpts
optsToUnitOpts m uninits = o1
  where o1 = unitOpts0 { uoLiterals = m
                       , uninitializeds = uninits
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
-- singlePfUnits'
--   :: UnitAnalysis a -> UnitOpts
--   -> AnalysisProgram () () IO [ProgramFile] a
-- singlePfUnits' unitAnalysis opts (pf:_) =
--   let ue = UnitEnv
--         { unitOpts = opts
--         , unitProgramFile = pf
--         }
--   in runUnitAnalysis ue unitAnalysis
-- singlePfUnits' _ _ [] = error "singlePfUnits': no program file provided"

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

unitsDump :: LiteralsOpt -> Bool -> CamfortEnv -> IO Int
unitsDump _ uninits env = do
  let modFileName = ceInputSources env
  modData <- LB.readFile modFileName
  let eResult = FM.decodeModFile modData
  case eResult of
    Left msg -> do
      putStrLn $ modFileName ++ ": Error: " ++ show msg
      pure 1
    Right modFiles -> do
      forM_ modFiles $ \ modFile -> do
        putStrLn $ modFileName ++ ": successfully parsed summary file."
        putStrLn . fromMaybe "unable to find units info" $ dumpModFileCompiledUnits modFile
      pure 0

unitsCheck :: LiteralsOpt -> Bool -> CamfortEnv -> IO Int
unitsCheck =
  runUnitsFunctionalityP
  "Checking units for"
  (singlePfUnits checkUnits)
  (describePerFileAnalysisP "unit checking")


unitsInfer :: Bool -> LiteralsOpt -> Bool -> CamfortEnv -> IO Int
unitsInfer showAST =
  runUnitsFunctionalityP
  "Inferring units for"
  (singlePfUnits inferUnits)
  (if showAST
      then describePerFileAnalysisShowASTUnitInfoP
      else describePerFileAnalysisP "unit inference")

-- | Given an analysis program for a single file, run it over every input file
-- and collect the reports, then print those reports to standard output.
describePerFileAnalysisShowASTUnitInfoP
  :: (MonadIO m, Describe w, Describe e, NFData w, NFData e)
  => AnalysisRunnerP e w m ProgramFile InferenceResult (AnalysisReport e w InferenceResult)
describePerFileAnalysisShowASTUnitInfoP program logOutput logLevel snippets modFiles =
  runPerFileAnalysisP program logOutput logLevel snippets modFiles >->
    (P.mapM $ \ r -> do
        case r of
          AnalysisReport _ _ (ARSuccess (Inferred (InferenceReport pfUA _))) -> do
            liftIO . pp . fmap (unitInfo . FA.prevAnnotation) . FA.rename $ pfUA
          _ -> pure ()
        putDescribeReport "unit inference" (Just logLevel) snippets r
        pure r)

-- | Expand all paths that are directories into a list of Fortran
-- files from a recursive directory listing.
expandDirs :: [FilePath] -> IO [FilePath]
expandDirs = fmap concat . mapM doEach
  where
    doEach path = do
      isDir <- doesDirectoryExist path
      if isDir
        then listFortranFiles path
        else pure [path]

-- | Get a list of Fortran files under the given directory.
listFortranFiles :: FilePath -> IO [FilePath]
listFortranFiles dir = filter isFortran <$> listDirectoryRecursively dir
  where
    -- | True if the file has a valid fortran extension.
    isFortran :: FilePath -> Bool
    isFortran x = map toLower (takeExtension x) `elem` exts
      where exts = [".f", ".f90", ".f77", ".f03"]

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = listDirectoryRec dir ""
  where
    listDirectoryRec :: FilePath -> FilePath -> IO [FilePath]
    listDirectoryRec d f = do
      let fullPath = d </> f
      isDir <- doesDirectoryExist fullPath
      if isDir
      then do
        conts <- listDirectory fullPath
        concat <$> mapM (listDirectoryRec fullPath) conts
      else pure [fullPath]

decodeOneModFile :: FilePath -> IO FM.ModFiles
decodeOneModFile path = do
  contents <- LB.readFile path
  case FM.decodeModFile contents of
    Left msg -> do
      hPutStrLn stderr $ path ++ ": Error: " ++ msg
      return []
    Right modFiles -> do
      hPutStrLn stderr $ path ++ ": successfully parsed summary file."
      return modFiles

unitsCompile :: LiteralsOpt -> Bool -> CamfortEnv -> IO Int
unitsCompile opts uninits env = do
  let uo = optsToUnitOpts opts uninits
  let description = "Compiling units for"
  putStrLn $ description ++ " '" ++ ceInputSources env ++ "'"
  incDir' <- maybe getCurrentDirectory pure (ceIncludeDir env)
  isDir <- doesDirectoryExist incDir'
  let incDir | isDir     = incDir'
             | otherwise = takeDirectory incDir'
  -- modFileNames <- getModFiles incDir

  paths' <- expandDirs $ [ceInputSources env]
  -- Build the graph of module dependencies
  mg0 <- FM.genModGraph (ceFortranVersion env) [incDir] Nothing paths'

  let compileFileToMod mods pf = do
        mod <- compileUnits uo mods pf
        let mfname = replaceExtension (FM.moduleFilename mod) FM.modFileSuffix
        LB.writeFile mfname (FM.encodeModFile [mod])
        pure mod

  -- Loop through the dependency graph until it is empty
  let loop mg mods
        | nxt <- FM.takeNextMods mg
        , not (null nxt) = do
            let fnPaths = [ fn | (_, Just (FM.MOFile fn)) <- nxt ]
            newMods <- fmap concat . forM fnPaths $ \ fnPath -> do
              tsStatus <- FM.checkTimestamps fnPath
              case tsStatus of
                FM.NoSuchFile -> do
                  putStr $ "Does not exist: " ++ fnPath
                  pure [FM.emptyModFile]
                FM.ModFileExists modPath -> do
                  putStrLn $ "Loading mod file " ++ modPath ++ "."
                  decodeOneModFile modPath
                FM.CompileFile -> do
                  putStr $ "Summarising " ++ fnPath ++ "..."
                  m_pf <- readParseSrcFile (ceFortranVersion env) mods fnPath
                  case m_pf of
                    Just (pf, _) -> do
                      mod <- compileFileToMod mods pf
                      putStrLn "done"
                      pure [mod]
                    Nothing -> do
                      putStrLn "failed"
                      pure []

            let ns  = map fst nxt
            let mg' = FM.delModNodes ns mg
            loop mg' $ newMods ++ mods
      loop _ mods = pure mods

  _allMods <- loop mg0 []
  return 0

unitsSynth :: AnnotationType -> FileOrDir -> LiteralsOpt -> Bool -> CamfortEnv -> IO Int
unitsSynth annType outSrc opts uninits env =
  runFunctionality "Synthesising units for"
                   (multiPfUnits (LU.synthesiseUnits (markerChar annType)) uo)
                   (doRefactor "unit synthesis" (ceInputSources env) outSrc)
                   compileUnits
                   uo
                   env
  where uo = optsToUnitOpts opts uninits

unitsCriticals :: LiteralsOpt -> Bool -> CamfortEnv -> IO Int
unitsCriticals opts uninits env =
  runUnitsFunctionalityP
  "Suggesting variables to annotate with unit specifications in"
  (singlePfUnits (inferCriticalVariables localPath))
  (describePerFileAnalysisP "unit critical variable analysis") opts uninits env
  where
    localPath = takeDirectory (ceInputSources env)

{- Stencils feature -}


stencilsCheck :: CamfortEnv -> IO Int
stencilsCheck =
  runFunctionalityP
  "Checking stencil specs for"
  (generalizePureAnalysis . Stencils.check)
  (describePerFileAnalysisP "stencil checking")
  compileStencils ()


stencilsInfer :: Bool -> CamfortEnv -> IO Int
stencilsInfer useEval =
  runFunctionalityP
  "Inferring stencil specs for"
  (generalizePureAnalysis . Stencils.infer useEval '=')
  (describePerFileAnalysisP "stencil inference")
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
  runFunctionalityP
  "Checking invariants in"
  (Hoare.check pro)
  (describePerFileAnalysisP "invariant checking")
  simpleCompiler ()


-- | Initialize Camfort for the given project.
camfortInitialize :: FilePath -> IO ()
camfortInitialize projectDir =
  createDirectoryIfMissing False (projectDir </> ".camfort")
