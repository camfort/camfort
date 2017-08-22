{- |
Module      :  Camfort.Input
Description :  Handles input of code base and passing the files on to core functionality.
Copyright   :  Copyright 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
-}

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Camfort.Input
  (
    -- * Classes
    Default(..)
    -- * Datatypes and Aliases
  , ProgramFile
  , AnalysisProgram
  , AnalysisRunner
    -- * Builders for analysers and refactorings
  , runPerFileAnalysis
  , runMultiFileAnalysis
  , describePerFileAnalysis
  , doRefactor
  , doRefactorAndCreate
  , perFileRefactoring
    -- * Source directory and file handling
  , readParseSrcDir
    -- * Combinators
  , runThen
  ) where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8         as B
import           Data.Either                   (partitionEithers)
import           Data.List                     (intercalate)

import           Control.Lens

import qualified Language.Fortran.AST          as F
import           Language.Fortran.Util.ModFile (ModFiles)

import           Camfort.Analysis
import           Camfort.Analysis.Logger
  -- (Analysis, analysisDebug, analysisResult, runAnalysis)
import           Camfort.Analysis.Annotations
import           Camfort.Analysis.ModFile      (MFCompiler, genModFiles,
                                                readParseSrcDir)
import           Camfort.Helpers
import           Camfort.Output

type AnalysisProgram e w m a b = a -> AnalysisT e w m b

type AnalysisRunner e w m a b r =
  AnalysisProgram e w m a b -> LogOutput m -> LogLevel -> ModFiles -> [(ProgramFile, SourceText)] -> m r

--------------------------------------------------------------------------------
--  Simple runners
--------------------------------------------------------------------------------

runPerFileAnalysis
  :: (Monad m, Describe e, Describe w)
  => AnalysisRunner e w m ProgramFile b [AnalysisReport e w b]
runPerFileAnalysis program logOutput logLevel modFiles =
  traverse (\pf ->
    runAnalysisT
      (F.pfGetFilename pf)
      logOutput
      logLevel
      modFiles
      (program pf)) . map fst

runMultiFileAnalysis
  :: (Monad m, Describe e, Describe w)
  => AnalysisRunner e w m [ProgramFile] b (AnalysisReport e w b)
runMultiFileAnalysis program logOutput logLevel modFiles
  = runAnalysisT "<unknown>" logOutput logLevel modFiles . program . map fst

--------------------------------------------------------------------------------
--  Complex Runners
--------------------------------------------------------------------------------

describePerFileAnalysis ::
  (MonadIO m, Describe r, Describe w, Describe e) =>
  AnalysisRunner e w m ProgramFile r ()
describePerFileAnalysis = runPerFileAnalysis `runThen` mapM_ (putDescribeReport Nothing)


doRefactor
  :: (Describe e, Describe e', Describe w, Describe r)
  => FileOrDir -> FilePath
  -> AnalysisRunner e w IO [ProgramFile] (r, [Either e' ProgramFile]) ()
doRefactor inSrc outSrc program logOutput logLevel modFiles pfsTexts = do
  report <- runMultiFileAnalysis program logOutput logLevel modFiles pfsTexts

  let
    -- Get the user-facing output from the report
    report' = fmap fst report
    -- Get the refactoring result form the report
    resultFiles = report ^? arResult . _ARSuccess . _2

  putDescribeReport Nothing report'

  -- If the refactoring succeeded, change the files
  case resultFiles of
    Just fs -> finishRefactor inSrc outSrc (map snd pfsTexts) fs
    Nothing -> return ()

doRefactorAndCreate
  :: (Describe e, Describe w)
  => FileOrDir -> FilePath
  -> AnalysisRunner e w IO [ProgramFile] ([ProgramFile], [ProgramFile]) ()
doRefactorAndCreate inSrc outSrc program logOutput logLevel modFiles pfsTexts = do
  report <- runMultiFileAnalysis program logOutput logLevel modFiles pfsTexts

  let
    -- Get the user-facing output from the report
    report' = fmap (const ()) report
    -- Get the refactoring result form the report
    resultFiles = report ^? arResult . _ARSuccess

  putDescribeReport Nothing report'

  case resultFiles of
    -- If the refactoring succeeded, change the files
    Just fs -> finishRefactorAndCreate inSrc outSrc (map snd pfsTexts) fs
    Nothing -> return ()

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

runThen
  :: (Monad m)
  => AnalysisRunner e w m a b r -> (r -> m r')
  -> AnalysisRunner e w m a b r'
runThen runner withResult program output level modFiles programFiles =
  runner program output level modFiles programFiles >>= withResult

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
  => MFCompiler r m -> r
  -> FileOrDir -- ^ Input source file or directory
  -> FileOrDir -- ^ Include path
  -> [Filename] -- ^ Excluded files
  -> m (ModFiles, [(ProgramFile, SourceText)])
loadModAndProgramFiles mfc env inSrc incDir excludes = do
  liftIO $ printExcludes inSrc excludes
  modFiles <- genModFiles mfc env incDir excludes
  ps <- liftIO $ readParseSrcDir modFiles inSrc excludes
  pure (modFiles, ps)

