{- |
Module      :  Camfort.Input
Description :  Handles input of code base and passing the files on to core functionality.
Copyright   :  Copyright 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
-}

{-# LANGUAGE DoAndIfThenElse #-}

module Camfort.Input where
  -- (
  --   -- * Classes
  --   Default(..)
  --   -- * Datatypes and Aliases
  -- , FileProgram
  --   -- * Builders for analysers and refactorings
  -- , doAnalysisReport
  -- , doRefactor
  -- , doRefactorAndCreate
  --   -- * Source directory and file handling
  -- , readParseSrcDir
  -- ) where

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


type RefactorResult e a = (a, [Either e ProgramFile])


type AnalysisProgram e w m a b = ModFiles -> a -> AnalysisT e w m b


runPerFileAnalysis
  :: (Monad m, Describe e, Describe w)
  => AnalysisProgram e w m ProgramFile b
  -> LogOutput m
  -> LogLevel
  -> ModFiles
  -> [ProgramFile]
  -> m [AnalysisReport e w b]
runPerFileAnalysis program logOutput logLevel modFiles =
  traverse $ \pf ->
    runAnalysisT
      (F.pfGetFilename pf)
      logOutput
      logLevel
      (program modFiles pf)


runMultiFileAnalysis
  :: (Monad m, Describe e, Describe w)
  => AnalysisProgram e w m [a] b
  -> LogOutput m
  -> LogLevel
  -> ModFiles
  -> [a]
  -> m (AnalysisReport e w b)
runMultiFileAnalysis program logOutput logLevel modFiles
  = runAnalysisT "<unknown>" logOutput logLevel . program modFiles



-- doRefactor :: []



-- -- * Builders for analysers and refactorings

-- -- | Alias for functions that run an 'Analysis' using a standard argument
-- -- format.
-- type AnalysisRunner r w a b f =
--   Analysis r w () a b -> MFCompiler r -> r
--   -> FileOrDir -> FileOrDir -> [Filename]
--   -> IO f

-- -- | Alias for 'AnalysisRunner' with an output source.
-- type AnalysisRunnerWithOut r w a b f =
--   Analysis r w () a b -> MFCompiler r -> r
--   -> FileOrDir -> FileOrDir -> [Filename]
--   -> FileOrDir
--   -> IO f

-- -- | Perform an analysis which reports to the user, but does not output any files.
-- doAnalysisReport
--   :: (Monoid w, Show w, Show b)
--   => AnalysisRunner r w FileProgram b ()
-- doAnalysisReport rFun mfc env inSrc incDir excludes = do
--   results <- doInitAnalysis' rFun mfc env inSrc incDir excludes
--   let report = concatMap (\(rep,res) -> show rep ++ show res) results
--   putStrLn report

-- doInitAnalysis
--   :: (Monoid w)
--   => AnalysisRunner r w [FileProgram] b ([(FileProgram, B.ByteString)], w, b)
-- doInitAnalysis analysis mfc env inSrc incDir excludes = do
--   (modFiles, ps) <- getModsAndPs mfc env inSrc incDir excludes
--   res <- runAnalysis analysis env () modFiles . fmap fst $ ps
--   let report = analysisDebug res
--       ps' = analysisResult res
--   pure (ps, report, ps')

-- doInitAnalysis'
--   :: (Monoid w)
--   => AnalysisRunner r w FileProgram b [(w, b)]
-- doInitAnalysis' analysis mfc env inSrc incDir excludes = do
--   (modFiles, ps) <- getModsAndPs mfc env inSrc incDir excludes
--   res <- traverse (runAnalysis analysis env () modFiles . fst) ps
--   pure $ fmap (\r -> (analysisDebug r, analysisResult r)) res

-- doRefactor
--   :: (Monoid w, Show w, Show e, Show b)
--   => AnalysisRunnerWithOut r w [FileProgram] (b, [Either e FileProgram]) ()
-- doRefactor rFun mfc env inSrc incDir excludes outSrc = do
--   (ps, report1, aRes) <- doInitAnalysis rFun mfc env inSrc incDir excludes
--   let (_, ps') = partitionEithers (snd aRes)
--       report = show report1 ++ show (fst aRes)
--   let outputs = reassociateSourceText (fmap snd ps) ps'
--   outputFiles inSrc outSrc outputs
--   putStrLn report

-- -- | Perform a refactoring that may create additional files.
-- doRefactorAndCreate
--   :: (Monoid w, Show w)
--   => AnalysisRunnerWithOut r w [FileProgram] ([FileProgram], [FileProgram]) ()
-- doRefactorAndCreate rFun mfc env inSrc incDir excludes outSrc = do
--   (ps, report, (ps', ps'')) <- doInitAnalysis rFun mfc env inSrc incDir excludes
--   let outputs = reassociateSourceText (fmap snd ps) ps'
--   let outputs' = map (\pf -> (pf, B.empty)) ps''
--   outputFiles inSrc outSrc outputs
--   outputFiles inSrc outSrc outputs'
--   print report 
