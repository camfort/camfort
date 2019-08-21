{-# LANGUAGE TemplateHaskell #-}

module Camfort.Analysis.TestUtils
  (
    -- * Inputs
    TestInput
  , testInputSources
  , tiInputSources
  , tiExcludeFiles
  , tiIncludeDir
    -- * Running Tests
  , testSingleFileAnalysis
  , testMultiFileAnalysis
  , testMultiFileAnalysisWithSrc
  ) where

import           Camfort.Analysis
import           Camfort.Analysis.ModFile
import           Camfort.Helpers
import           Camfort.Input
import           Control.Lens
import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import qualified Language.Fortran.AST as F
import           Language.Fortran.Util.ModFile (ModFiles)
import           System.Directory (getCurrentDirectory)
import           System.FilePath


data TestInput =
  TestInput
  { _tiInputSources :: FileOrDir
  , _tiExcludeFiles :: [Filename]
  , _tiIncludeDir   :: Maybe FilePath
  }

makeLenses ''TestInput

testInputSources :: FileOrDir -> TestInput
testInputSources inputSources =
  TestInput
  { _tiInputSources = inputSources
  , _tiExcludeFiles = []
  , _tiIncludeDir = Just $ takeDirectory inputSources
  }


loadInput :: TestInput -> IO (ModFiles, [(ProgramFile, SourceText)])
loadInput input = do
  incDir <- case input ^. tiIncludeDir of
    Just x  -> return x
    Nothing -> getCurrentDirectory

  modFiles <- getModFiles incDir
  pfsTexts <- readParseSrcDir Nothing modFiles (input ^. tiInputSources) (input ^. tiExcludeFiles)
  return (modFiles, pfsTexts)


testSingleFileAnalysis
  :: (Describe e, Describe w, MonadIO m)
  => TestInput
  -> AnalysisProgram e w IO ProgramFile b
  -> (AnalysisReport e w b -> m ())
  -> m ()
testSingleFileAnalysis input program testReport = do
  (mfs, pfsSources) <- liftIO $ loadInput input

  forM_ pfsSources $ \ (pf, _) -> do
    report <- liftIO $
      runAnalysisT
      (F.pfGetFilename pf)
      (logOutputNone True)
      LogError
      mfs
      (program pf)
    testReport report


testMultiFileAnalysis
  :: (Describe e, Describe w, MonadIO m)
  => TestInput
  -> AnalysisProgram e w IO [ProgramFile] b
  -> (AnalysisReport e w b -> m ())
  -> m ()
testMultiFileAnalysis input program = testMultiFileAnalysisWithSrc input program . const


testMultiFileAnalysisWithSrc
  :: (Describe e, Describe w, MonadIO m)
  => TestInput
  -> AnalysisProgram e w IO [ProgramFile] b
  -> ([SourceText] -> AnalysisReport e w b -> m ())
  -> m ()
testMultiFileAnalysisWithSrc input program testReport = do
  (mfs, (pfs, sources)) <- fmap (over _2 unzip) $ liftIO $ loadInput input

  let fname = maybe "" F.pfGetFilename (pfs ^? _head)

  report <- liftIO $
    runAnalysisT
    fname
    (logOutputNone True)
    LogError
    mfs
    (program pfs)
  testReport sources report
