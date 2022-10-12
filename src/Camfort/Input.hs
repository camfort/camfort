{- |
Module      :  Camfort.Input
Description :  Handles input of code base and passing the files on to core functionality.
Copyright   :  Copyright 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
-}

module Camfort.Input
  (
    -- * Classes
    Default(..)
    -- * Datatypes and Aliases
  , ProgramFile
  , AnalysisProgram
  , AnalysisRunner
  , AnalysisRunnerP
  , AnalysisRunnerConsumer
    -- * Builders for analysers and refactorings
  , runPerFileAnalysisP
  , runMultiFileAnalysis
  , describePerFileAnalysisP
  , doRefactor
  , doRefactorAndCreate
  , perFileRefactoring
    -- * Source directory and file handling
  , readParseSrcDir
  , loadModAndProgramFiles
    -- * Combinators
  , runThen
  ) where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8         as B
import           Data.Either                   (partitionEithers)
import           Data.List                     (intercalate)

import           Control.Lens
import           Control.DeepSeq

import qualified Language.Fortran.AST          as F
import           Language.Fortran.Util.ModFile (ModFiles, emptyModFiles)
import           Language.Fortran.Version      (FortranVersion(..))

import           Camfort.Analysis
import           Camfort.Analysis.Annotations
import           Camfort.Analysis.Logger
import           Camfort.Analysis.ModFile      (MFCompiler, genModFiles, readParseSrcDir)
import           Camfort.Helpers
import           Camfort.Output

import           Pipes
import qualified Pipes.Prelude                 as P

-- | An analysis program which accepts inputs of type @a@ and produces results
-- of type @b@.
--
-- Has error messages of type @e@ and warnings of type @w@. Runs in the base
-- monad @m@.
type AnalysisProgram e w m a b = a -> AnalysisT e w m b

-- | An 'AnalysisRunner' is a function to run an 'AnalysisProgram' in a
-- particular way. Produces a final result of type @r@.
type AnalysisRunner e w m a b r =
  AnalysisProgram e w m a b -> LogOutput m -> LogLevel -> Bool -> ModFiles -> [(ProgramFile, SourceText)] -> m r

type AnalysisRunnerP e w m a b r =
  AnalysisProgram e w m a b -> LogOutput m -> LogLevel -> Bool -> ModFiles -> Pipe (ProgramFile, SourceText) r m ()

type AnalysisRunnerConsumer e w m a b r =
  AnalysisProgram e w m a b -> LogOutput m -> LogLevel -> Bool -> ModFiles -> Consumer (ProgramFile, SourceText) m ()

--------------------------------------------------------------------------------
--  Simple runners
--------------------------------------------------------------------------------

-- | Given an analysis program for a single file, run it over every input file
-- and collect the reports. Doesn't produce any output.
runPerFileAnalysisP
  :: (MonadIO m, Describe e, Describe w, NFData e, NFData w, NFData b)
  => AnalysisRunnerP e w m ProgramFile b (AnalysisReport e w b)
runPerFileAnalysisP program logOutput logLevel _ modFiles =
  P.mapM $ \ (pf, _) -> do
    -- liftIO . putStrLn $ "Running analysis on " ++ (F.pfGetFilename pf)
    runAnalysisT (F.pfGetFilename pf)
                 logOutput
                 logLevel
                 modFiles
                 (program pf)

-- | Run an analysis program over every input file and get the report. Doesn't
-- produce any output.
runMultiFileAnalysis
  :: (Monad m, Describe e, Describe w)
  => AnalysisRunner e w m [ProgramFile] b (AnalysisReport e w b)
runMultiFileAnalysis program logOutput logLevel _ modFiles
  = runAnalysisT "<unknown>" logOutput logLevel modFiles . program . map fst

--------------------------------------------------------------------------------
--  Complex Runners
--------------------------------------------------------------------------------

-- doCreateBinary
--   :: (MonadIO m, Describe r, Describe w, Describe e)
--   => Text -> AnalysisRunner e w m ProgramFile r ()
-- doCreateBinary analysisName = runPerFileAnalysis `runThen` writeCompiledFiles
--   where
--     writeCompiledFiles :: (r, [(Filename, B.ByteString)]) -> IO r
--     writeCompiledFiles (report, bins) = do
--       outputFiles inSrc outSrc bins
--       pure report

-- FIXME
{-
compilePerFile :: (Describe e, Describe e', Describe w, Describe r) =>
                  Text
               -> FileOrDir
               -> FilePath
               -> AnalysisRunner e w IO [ProgramFile] (r, [Either e' ProgramFile]) ()
compilePerFile analysisName inSrc outSrc =
    runPerFileAnalysis `runThen` writeCompiledFiles
  where
    writeCompiledFiles :: (r, [(Filename, B.ByteString)]) -> IO r
    writeCompiledFiles (report, bins) = do
      outputFiles inSrc outSrc bins
      pure report
-}

-- | Given an analysis program for a single file, run it over every input file
-- and collect the reports, then print those reports to standard output.
describePerFileAnalysisP
  :: (MonadIO m, Describe r, ExitCodeOfReport r, Describe w, Describe e, NFData e, NFData w, NFData r)
  => Text -> AnalysisRunnerP e w m ProgramFile r (AnalysisReport e w r)
describePerFileAnalysisP analysisName program logOutput logLevel snippets modFiles = do
  runPerFileAnalysisP program logOutput logLevel snippets modFiles >->
    (P.mapM $ \ r -> liftIO (putDescribeReport analysisName (Just logLevel) snippets r) >> pure r)

-- | Accepts an analysis program for multiple input files which produces a
-- result value along with refactored files. Performs the refactoring, and
-- prints the result value with the report.
doRefactor
  :: (Describe e, Describe e', Describe w, Describe r, ExitCodeOfReport r)
  => Text
  -> FileOrDir -> FilePath
  -> AnalysisRunner e w IO [ProgramFile] (r, [Either e' ProgramFile]) Int
doRefactor analysisName inSrc outSrc program logOutput logLevel snippets modFiles pfsTexts = do
  report <- runMultiFileAnalysis program logOutput logLevel snippets modFiles pfsTexts

  let
    -- Get the user-facing output from the report
    report' = fmap fst report
    -- Get the refactoring result form the report
    resultFiles = report ^? arResult . _ARSuccess . _2

  putDescribeReport analysisName (Just logLevel) snippets report'

  -- If the refactoring succeeded, change the files
  case resultFiles of
    Just fs -> finishRefactor inSrc outSrc (map snd pfsTexts) fs >>
               return (exitCodeOf report')
    Nothing -> return (exitCodeOf report')

-- | Accepts an analysis program for multiple input files which produces
-- refactored files and creates new files. Performs the refactoring.
doRefactorAndCreate
  :: (Describe e, Describe w)
  => Text
  -> FileOrDir -> FilePath
  -> AnalysisRunner e w IO [ProgramFile] ([ProgramFile], [ProgramFile]) Int
doRefactorAndCreate analysisName inSrc outSrc program logOutput logLevel snippets modFiles pfsTexts = do
  report <- runMultiFileAnalysis program logOutput logLevel snippets modFiles pfsTexts

  let
    -- Get the user-facing output from the report
    report' = fmap (const ()) report
    -- Get the refactoring result form the report
    resultFiles = report ^? arResult . _ARSuccess

  putDescribeReport analysisName (Just logLevel) snippets report'

  case resultFiles of
    -- If the refactoring succeeded, change the files
    Just fs -> finishRefactorAndCreate inSrc outSrc (map snd pfsTexts) fs >>
               return (exitCodeOf report')
    Nothing -> return (exitCodeOf report')

-- | Accepts an analysis program to refactor a single file and returns an
-- analysis program to refactor each input file with that refactoring.
perFileRefactoring
  :: (Monad m)
  => AnalysisProgram e w m ProgramFile ProgramFile
  -> AnalysisProgram e w m [ProgramFile] ((), [Either e ProgramFile])
perFileRefactoring program pfs = do
  pfs' <- mapM program pfs
  return ((), fmap pure pfs')

--------------------------------------------------------------------------------
--  Refactoring Combinators
--------------------------------------------------------------------------------

finishRefactor
  :: FileOrDir -> FilePath
  -> [SourceText]
  -- ^ Original source from the input files
  -> [Either e ProgramFile]
  -- ^ Changed input files (or errors)
  -> IO ()
finishRefactor inSrc outSrc inputText analysisOutput = do
  let (_, ps') = partitionEithers analysisOutput
      outputs = reassociateSourceText inputText ps'

  outputFiles inSrc outSrc outputs


finishRefactorAndCreate
  :: FileOrDir -> FilePath
  -> [SourceText]
  -- ^ Original source from the input files
  -> ([ProgramFile], [ProgramFile])
  -- ^ Changed input files, newly created files
  -> IO ()
finishRefactorAndCreate inSrc outSrc inputText analysisOutput = do

  let changedFiles = reassociateSourceText inputText (fst analysisOutput)
      newFiles = map (\pf -> (pf, B.empty)) (snd analysisOutput)

  outputFiles inSrc outSrc changedFiles
  outputFiles inSrc outSrc newFiles

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------

-- | Monadic bind for analysis runners.
runThen
  :: (Monad m)
  => AnalysisRunner e w m a b r -> (r -> m r')
  -> AnalysisRunner e w m a b r'
runThen runner withResult program output level snippets modFiles programFiles =
  runner program output level snippets modFiles programFiles >>= withResult

--------------------------------------------------------------------------------
--  Misc
--------------------------------------------------------------------------------

-- | Class for default values of some type 't'
class Default t where
    defaultValue :: t

-- | Print a string to the user informing them of files excluded
-- from the operation.
printExcludes :: Filename -> [Filename] -> IO ()
printExcludes _ []           = pure ()
printExcludes _ [""]         = pure ()
printExcludes inSrc excludes =
  putStrLn $ concat ["Excluding ", intercalate "," excludes, " from ", inSrc, "/"]


-- | For refactorings which create additional files.
type ProgramFile = F.ProgramFile A

reassociateSourceText
  :: [SourceText]
  -> [F.ProgramFile Annotation]
  -> [(F.ProgramFile Annotation, SourceText)]
reassociateSourceText ps ps' = zip ps' ps


loadModAndProgramFiles
  :: (MonadIO m)
  => Maybe FortranVersion
  -> MFCompiler r m -> r
  -> FileOrDir -- ^ Input source file or directory
  -> FileOrDir -- ^ Include path
  -> [Filename] -- ^ Excluded files
  -> m (ModFiles, [(ProgramFile, SourceText)])
loadModAndProgramFiles mv mfc env inSrc incDir excludes = do
  liftIO $ printExcludes inSrc excludes
  modFiles <- genModFiles mv emptyModFiles mfc env incDir excludes
  ps <- liftIO $ readParseSrcDir mv modFiles inSrc excludes
  pure (modFiles, ps)
