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
  , callAndSummarise
  , doAnalysisReportWithModFiles
  , doAnalysisSummary
  , doRefactor
  , doRefactorAndCreate
  , doRefactorWithModFiles
    -- * Source directory and file handling
  , doCreateBinary
  , readParseSrcDir
  , getModFilesWithNames
  ) where

import           Control.Monad (forM)
import           Data.Binary (decodeFileOrFail)
import qualified Data.ByteString.Char8 as B
import           Data.Char (toUpper)
import           Data.List (foldl', (\\), intercalate)
import           Data.Maybe
import           Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error (replace)
import           System.Directory
import           System.FilePath ((</>), takeExtension)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Parser.Any as FP
import           Language.Fortran.Util.ModFile

import Camfort.Analysis.Annotations
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
  => (Filename -> F.ProgramFile A -> (s, F.ProgramFile A))
  -> FileOrDir -> [Filename] -> IO ()
doAnalysisSummary aFun inSrc excludes = do
  printExcludes inSrc excludes
  ps <- readParseSrcDir inSrc excludes
  let (out, _) = callAndSummarise aFun ps
  putStrLn . show' $ out

-- | Perform an analysis that produces information of type @s@.
callAndSummarise :: (Monoid s)
  => (Filename -> F.ProgramFile A -> (s, a))
  -> [(Filename, SourceText, F.ProgramFile A)]
  -> (s, [a])
callAndSummarise aFun =
  foldl' (\(n, pss) (f, _, ps) ->
            let (n', ps') = aFun f ps
            in (n `mappend` n', ps' : pss)) (mempty, [])

-- | Perform an analysis which reports to the user, but does not output any files.
doAnalysisReportWithModFiles
  :: ([(Filename, SourceText, F.ProgramFile A)] -> r)
  -> (r -> IO out)
  -> FileOrDir
  -> Maybe FileOrDir
  -> [Filename]
  -> IO out
doAnalysisReportWithModFiles rFun sFun inSrc incDir excludes = do
  printExcludes inSrc excludes
  ps <- readParseSrcDirWithModFiles inSrc incDir excludes

  let report = rFun ps
  sFun report

-- | Perform a refactoring that does not add any new files.
doRefactor :: ([FileProgram]
           -> (String, [FileProgram]))
           -> FileOrDir -> [Filename] -> FileOrDir
           -> IO String
doRefactor rFun inSrc excludes outSrc =
  let rFunOnModFiles = (rFun . fmap (\(fn, _, fp) -> (fn, fp)))
  in doRefactorWithModFiles rFunOnModFiles inSrc Nothing excludes outSrc

doRefactorWithModFiles
  :: ([(Filename, SourceText, F.ProgramFile A)] -> (String, [FileProgram]))
  -> FileOrDir
  -> Maybe FileOrDir
  -> [Filename]
  -> FileOrDir
  -> IO String
doRefactorWithModFiles rFun inSrc incDir excludes outSrc = do
  printExcludes inSrc excludes
  ps <- readParseSrcDirWithModFiles inSrc incDir excludes
  let (report, ps') = rFun ps
  let outputs = reassociateSourceText ps ps'
  outputFiles inSrc outSrc outputs
  pure report

-- | Perform a refactoring that may create additional files.
doRefactorAndCreate
  :: ([FileProgram] -> (String, [FileProgram], [FileProgram]))
  -> FileOrDir -> [Filename] -> FileOrDir -> IO String
doRefactorAndCreate rFun inSrc excludes outSrc = do
  printExcludes inSrc excludes
  ps <- readParseSrcDir inSrc excludes
  let (report, ps', ps'') = rFun (fmap (\(f, _, ast) -> (f, ast)) ps)
  let outputs = reassociateSourceText ps ps'
  let outputs' = map (\(f, pf) -> (f, B.empty, pf)) ps''
  outputFiles inSrc outSrc outputs
  outputFiles inSrc outSrc outputs'
  pure report

-- | For refactorings which create additional files.
type FileProgram = (Filename, F.ProgramFile A)

doCreateBinary
  :: ([FileProgram] -> (String, [(Filename, B.ByteString)]))
  -> FileOrDir
  -> Maybe FileOrDir
  -> [Filename]
  -> FileOrDir
  -> IO String
doCreateBinary rFun inSrc incDir excludes outSrc = do
  printExcludes inSrc excludes
  ps <- readParseSrcDirWithModFiles inSrc incDir excludes
  let (report, bins) = rFun (map (\ (f, _, ast) -> (f, ast)) ps)
  outputFiles inSrc outSrc bins
  pure report

reassociateSourceText :: [(Filename, SourceText, a)]
                   -> [(Filename, F.ProgramFile Annotation)]
                   -> [(Filename, SourceText, F.ProgramFile Annotation)]
reassociateSourceText ps ps' = zip3 (map fst ps') (map snd3 ps) (map snd ps')
  where snd3 (_, b, _) = b

-- * Source directory and file handling

-- | Read files from a directory.
readParseSrcDir :: FileOrDir  -- ^ Directory to read from.
                -> [Filename] -- ^ Excluded files.
                -> IO [(Filename, SourceText, F.ProgramFile A)]
readParseSrcDir inp excludes =
  readParseSrcDirWithModFiles inp Nothing excludes

readParseSrcDirWithModFiles :: FileOrDir
                            -> Maybe FileOrDir
                            -> [Filename]
                            -> IO [(Filename, SourceText, F.ProgramFile A)]
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

readParseSrcFileWithModFiles :: Maybe FileOrDir
                             -> Filename
                             -> IO (Maybe (Filename, SourceText, F.ProgramFile A))
readParseSrcFileWithModFiles incDir f = do
  inp <- flexReadFile f
  mods <- maybe (pure emptyModFiles) getModFiles incDir
  let result = FP.fortranParserWithModFiles mods inp f
  case result of
    Right ast -> pure $ Just (f, inp, fmap (const unitAnnotation) ast)
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
      if isDir then do
        fmap (fmap (path </>)) (rGetDirContents dPath)
      else pure [path]

-- | Retrieve a list of ModFiles from the directory, each associated
-- to the name of the file they are contained within.
getModFilesWithNames :: FileOrDir -> IO [(Filename, ModFile)]
getModFilesWithNames dir = do
  -- Figure out the camfort mod files and parse them.
  modFileNames <- filter isModFile <$> rGetDirContents dir
  forM modFileNames $ \ modFileName -> do
    eResult <- decodeFileOrFail (dir ++ "/" ++ modFileName) -- FIXME, directory manipulation
    case eResult of
      Left (offset, msg) -> do
        putStrLn $ modFileName ++ ": Error at offset " ++ show offset ++ ": " ++ msg
        pure (modFileName, emptyModFile)
      Right modFile -> do
        putStrLn $ modFileName ++ ": successfully parsed precompiled file."
        pure (modFileName, modFile)
  where
    isModFile :: Filename -> Bool
    isModFile = (== modFileSuffix) . takeExtension

-- | Retrieve the ModFiles from a directory.
getModFiles :: FileOrDir -> IO ModFiles
getModFiles = fmap (fmap snd) . getModFilesWithNames
