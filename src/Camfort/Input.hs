{- |
Module      :  Camfort.Input
Description :  Handles input of code base and passing the files on to core functionality.
Copyright   :  Copyright 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
-}

{-# LANGUAGE DoAndIfThenElse #-}

module Camfort.Input
  (
    -- * Classes
    Default(..)
    -- * Datatypes and Aliases
  , FileProgram
    -- * Builders for analysers and refactorings
  , doAnalysisReport
  , doRefactor
  , doRefactorAndCreate
    -- * Source directory and file handling
  , readParseSrcDir
  ) where

import qualified Data.ByteString.Char8 as B
import           Data.Either (partitionEithers)
import           Data.List (intercalate)

import qualified Language.Fortran.AST as F
import           Language.Fortran.Util.ModFile (ModFiles)

import Camfort.Analysis
  (Analysis, analysisDebug, analysisResult, runAnalysis)
import Camfort.Analysis.Annotations
import Camfort.Analysis.ModFile
  (MFCompiler, genModFiles, readParseSrcDir)
import Camfort.Helpers
import Camfort.Output

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

-- * Builders for analysers and refactorings

-- | Alias for functions that run an 'Analysis' using a standard argument
-- format.
type AnalysisRunner r w a b f =
  Analysis r w () a b -> MFCompiler r -> r
  -> FileOrDir -> FileOrDir -> [Filename]
  -> f

-- | Alias for 'AnalysisRunner' with an output source.
type AnalysisRunnerWithOut r w a b f =
  AnalysisRunner r w a b (FileOrDir -> f)

-- | Perform an analysis which reports to the user, but does not output any files.
doAnalysisReport
  :: (Monoid w, Show w, Show b)
  => AnalysisRunner r w FileProgram b (IO ())
doAnalysisReport rFun mfc env inSrc incDir excludes = do
  results <- doInitAnalysis' rFun mfc env inSrc incDir excludes
  let report = concatMap (\(rep,res) -> show rep ++ show res) results
  putStrLn report

getModsAndPs
  :: MFCompiler r -> r
  -> FileOrDir -> FileOrDir -> [Filename]
  -> IO (ModFiles, [(FileProgram, B.ByteString)])
getModsAndPs mfc env inSrc incDir excludes = do
  printExcludes inSrc excludes
  modFiles <- genModFiles mfc env inSrc incDir excludes
  ps <- readParseSrcDir modFiles inSrc excludes
  pure (modFiles, ps)

doInitAnalysis
  :: (Monoid w)
  => AnalysisRunner r w [FileProgram] b (IO ([(FileProgram, B.ByteString)], w, b))
doInitAnalysis analysis mfc env inSrc incDir excludes = do
  (modFiles, ps) <- getModsAndPs mfc env inSrc incDir excludes
  let res = runAnalysis analysis env () modFiles . fmap fst $ ps
      report = analysisDebug res
      ps' = analysisResult res
  pure (ps, report, ps')

doInitAnalysis'
  :: (Monoid w)
  => AnalysisRunner r w FileProgram b (IO [(w, b)])
doInitAnalysis' analysis mfc env inSrc incDir excludes = do
  (modFiles, ps) <- getModsAndPs mfc env inSrc incDir excludes
  let res = runAnalysis analysis env () modFiles . fst <$> ps
  pure $ fmap (\r -> (analysisDebug r, analysisResult r)) res

doRefactor
  :: (Monoid w, Show w, Show e, Show b)
  => AnalysisRunnerWithOut r w [FileProgram] (b, [Either e FileProgram]) (IO ())
doRefactor rFun mfc env inSrc incDir excludes outSrc = do
  (ps, report1, aRes) <- doInitAnalysis rFun mfc env inSrc incDir excludes
  let (_, ps') = partitionEithers (snd aRes)
      report = show report1 ++ show (fst aRes)
  let outputs = reassociateSourceText (fmap snd ps) ps'
  outputFiles inSrc outSrc outputs
  putStrLn report

-- | Perform a refactoring that may create additional files.
doRefactorAndCreate
  :: (Monoid w, Show w)
  => AnalysisRunnerWithOut r w [FileProgram] ([FileProgram], [FileProgram]) (IO ())
doRefactorAndCreate rFun mfc env inSrc incDir excludes outSrc = do
  (ps, report, (ps', ps'')) <- doInitAnalysis rFun mfc env inSrc incDir excludes
  let outputs = reassociateSourceText (fmap snd ps) ps'
  let outputs' = map (\pf -> (pf, B.empty)) ps''
  outputFiles inSrc outSrc outputs
  outputFiles inSrc outSrc outputs'
  print report

-- | For refactorings which create additional files.
type FileProgram = F.ProgramFile A

reassociateSourceText :: [SourceText]
                      -> [F.ProgramFile Annotation]
                      -> [(F.ProgramFile Annotation, SourceText)]
reassociateSourceText ps ps' = zip ps' ps
