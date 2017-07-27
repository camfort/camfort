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
  , doAnalysisReportWithModFiles
  , doAnalysisSummary
  , doRefactorAndCreate
  , doRefactorWithModFiles
    -- * Source directory and file handling
  , readParseSrcDirWithModFiles
  ) where

import qualified Data.ByteString.Char8 as B
import           Data.Char (toUpper)
import           Data.Either (partitionEithers)
import           Data.List ((\\), intercalate)
import           Data.Maybe
import           Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error (replace)
import           System.Directory
import           System.FilePath ((</>), takeExtension)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Parser.Any as FP

import Camfort.Analysis.Annotations
import Camfort.Analysis.Fortran
  (SimpleAnalysis, analysisDebug, analysisResult, runSimpleAnalysis)
import Camfort.Analysis.ModFile (getModFiles)
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

-- | Perform an analysis that produces information of type @s@.
doAnalysisSummary :: (Monoid s, Show' s)
  => SimpleAnalysis FileProgram s
  -> FileOrDir -> FileOrDir -> [Filename] -> IO ()
doAnalysisSummary aFun inSrc incDir excludes = do
  printExcludes inSrc excludes
  ps <- readParseSrcDirWithModFiles inSrc incDir excludes
  modFiles <- getModFiles incDir
  let results = runSimpleAnalysis aFun modFiles . fst <$> ps
      out = mconcat . fmap analysisResult $ results
  putStrLn . show' $ out

-- | Perform an analysis which reports to the user, but does not output any files.
doAnalysisReportWithModFiles
  :: (Show r)
  => SimpleAnalysis FileProgram r
  -> FileOrDir
  -> FileOrDir
  -> [Filename]
  -> IO ()
doAnalysisReportWithModFiles rFun inSrc incDir excludes = do
  printExcludes inSrc excludes
  ps <- readParseSrcDirWithModFiles inSrc incDir excludes
  modFiles <- getModFiles incDir
  let results = runSimpleAnalysis rFun modFiles . fst <$> ps
      report = concatMap (\r -> show (analysisDebug r) ++ show (analysisResult r)) results
  putStrLn report

doRefactorWithModFiles
  :: (Show e, Show r)
  => SimpleAnalysis [FileProgram] (r, [Either e FileProgram])
  -> FileOrDir
  -> FileOrDir
  -> [Filename]
  -> FileOrDir
  -> IO String
doRefactorWithModFiles rFun inSrc incDir excludes outSrc = do
  printExcludes inSrc excludes
  ps <- readParseSrcDirWithModFiles inSrc incDir excludes
  modFiles <- getModFiles incDir
  let res = runSimpleAnalysis rFun modFiles . fmap fst $ ps
      aRes = analysisResult res
      (_, ps') = partitionEithers (snd aRes)
      report = show (analysisDebug res) ++ show (fst aRes)
  let outputs = reassociateSourceText (fmap snd ps) ps'
  outputFiles inSrc outSrc outputs
  pure report

-- | Perform a refactoring that may create additional files.
doRefactorAndCreate
  :: SimpleAnalysis [FileProgram] ([FileProgram], [FileProgram])
  -> FileOrDir -> [Filename] -> FileOrDir -> FileOrDir -> IO Report
doRefactorAndCreate rFun inSrc excludes incDir outSrc = do
  printExcludes inSrc excludes
  ps <- readParseSrcDirWithModFiles inSrc incDir excludes
  modFiles <- getModFiles incDir
  let res = runSimpleAnalysis rFun modFiles . fmap fst $ ps
      report      = analysisDebug  res
      (ps', ps'') = analysisResult res
  let outputs = reassociateSourceText (fmap snd ps) ps'
  let outputs' = map (\pf -> (pf, B.empty)) ps''
  outputFiles inSrc outSrc outputs
  outputFiles inSrc outSrc outputs'
  pure report

-- | For refactorings which create additional files.
type FileProgram = F.ProgramFile A

reassociateSourceText :: [SourceText]
                      -> [F.ProgramFile Annotation]
                      -> [(F.ProgramFile Annotation, SourceText)]
reassociateSourceText ps ps' = zip ps' ps

-- * Source directory and file handling

readParseSrcDirWithModFiles :: FileOrDir
                            -> FileOrDir
                            -> [Filename]
                            -> IO [(FileProgram, SourceText)]
readParseSrcDirWithModFiles inp incDir excludes = do
  isdir <- isDirectory inp
  files <-
    if isdir
    then do
      files <- getFortranFiles inp
      -- Compute alternate list of excludes with the
      -- the directory appended
      let excludes' = excludes ++ map (\x -> inp ++ "/" ++ x) excludes
      pure $ map (\y -> inp ++ "/" ++ y) files \\ excludes'
    else pure [inp]
  mapMaybeM (readParseSrcFileWithModFiles incDir) files
  where
    mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
    mapMaybeM f = fmap catMaybes . mapM f

readParseSrcFileWithModFiles :: FileOrDir
                             -> Filename
                             -> IO (Maybe (FileProgram, SourceText))
readParseSrcFileWithModFiles incDir f = do
  inp <- flexReadFile f
  mods <- getModFiles incDir
  let result = FP.fortranParserWithModFiles mods inp f
  case result of
    Right ast -> pure $ Just (fmap (const unitAnnotation) ast, inp)
    Left  err -> print err >> pure Nothing
  where
    -- | Read file using ByteString library and deal with any weird characters.
    flexReadFile :: String -> IO B.ByteString
    flexReadFile = fmap (encodeUtf8 . decodeUtf8With (replace ' ')) . B.readFile

getFortranFiles :: FileOrDir -> IO [String]
getFortranFiles =
  fmap (filter isFortran) . rGetDirContents
  where
    -- | True if the file has a valid fortran extension.
    isFortran :: Filename -> Bool
    isFortran x = takeExtension x `elem` (exts ++ extsUpper)
      where exts = [".f", ".f90", ".f77", ".cmn", ".inc"]
            extsUpper = map (map toUpper) exts

-- | Recursively get the contents of a directory.
rGetDirContents :: FileOrDir -> IO [Filename]
rGetDirContents d = do
  ds <- listDirectory d
  fmap concat . mapM rGetDirContents' $ ds
  where
    -- | Get contents of directory if path points to a valid
    -- directory, otherwise return the path (a file).
    rGetDirContents' path = do
      let dPath = d </> path
      isDir <- doesDirectoryExist dPath
      if isDir then
        fmap (fmap (path </>)) (rGetDirContents dPath)
      else pure [path]
