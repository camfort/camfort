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
  ) where

import           Control.Monad                 (forM_)
import           System.Directory              (getCurrentDirectory)

import           Control.Lens

import qualified Language.Fortran.AST          as F
import           Language.Fortran.Util.ModFile (ModFiles)

import           Camfort.Analysis
import           Camfort.Analysis.ModFile
import           Camfort.Helpers
import           Camfort.Input

import           Test.Hspec


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
  , _tiIncludeDir = Nothing
  }


loadInput :: TestInput -> IO (ModFiles, [(ProgramFile, SourceText)])
loadInput input = do
  incDir <- case input ^. tiIncludeDir of
    Just x  -> return x
    Nothing -> getCurrentDirectory

  modFiles <- genModFiles simpleCompiler () incDir (input ^. tiExcludeFiles)
  pfsTexts <- readParseSrcDir modFiles (input ^. tiInputSources) (input ^. tiExcludeFiles)
  return (modFiles, pfsTexts)


testSingleFileAnalysis
  :: (Describe e, Describe w)
  => TestInput
  -> AnalysisProgram e w IO ProgramFile b
  -> (AnalysisReport e w b -> Spec)
  -> Spec
testSingleFileAnalysis input program testReport = do
  (mfs, pfsSources) <- runIO $ loadInput input

  forM_ pfsSources $ \ (pf, _) -> do
    report <- runIO $
      runAnalysisT
      (F.pfGetFilename pf)
      (const (return ()))
      LogError
      mfs
      (program pf)
    testReport report


testMultiFileAnalysis
  :: (Describe e, Describe w)
  => TestInput
  -> AnalysisProgram e w IO [ProgramFile] b
  -> (AnalysisReport e w b -> Spec)
  -> Spec
testMultiFileAnalysis input program testReport = do
  (mfs, (pfs, _)) <- fmap (over _2 unzip) $ runIO $ loadInput input

  let fname = maybe "" F.pfGetFilename (pfs ^? _head)

  report <- runIO $
    runAnalysisT
    fname
    (const (return ()))
    LogError
    mfs
    (program pfs)
  testReport report
