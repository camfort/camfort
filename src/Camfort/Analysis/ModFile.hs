{- |
Module      :  Camfort.Analysis.ModFile
Description :  CamFort-specific ModFiles helpers.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

module Camfort.Analysis.ModFile
  ( MFCompiler
  , genModFiles
  , getModFiles
  , readParseSrcDir
  , simpleCompiler
  , withCombinedEnvironment
  ) where

import           Control.Monad (forM)
import qualified Data.ByteString as B
import           Data.Char (toUpper)
import           Data.Data (Data)
import           Data.List ((\\))
import           Data.Maybe (catMaybes)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error (replace)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath ((</>), takeExtension)


import qualified Language.Fortran.AST               as F
import qualified Language.Fortran.Analysis          as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.Types    as FAT
import qualified Language.Fortran.Parser.Any        as FP
import           Language.Fortran.Util.ModFile

import Camfort.Analysis.Annotations (A, unitAnnotation)
import Camfort.Helpers

-- | Compiler for ModFile information.
type MFCompiler r = r -> ModFiles -> F.ProgramFile A -> IO ModFile

-- | Compile the Modfile with only basic information.
simpleCompiler :: MFCompiler ()
simpleCompiler () mfs = return . genModFile . withCombinedEnvironment mfs

-- | Normalize the 'ProgramFile' to include environment information from
-- the 'ModFiles'.
withCombinedEnvironment :: (Data a) => ModFiles -> F.ProgramFile a -> F.ProgramFile (FA.Analysis a)
withCombinedEnvironment mfs pf =
  let
    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap mfs
    tenv = combinedTypeEnv mfs
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis $ pf
  in fst . FAT.analyseTypesWithEnv tenv $ pfRenamed

genCModFile :: MFCompiler r -> r -> ModFiles -> F.ProgramFile A -> IO ModFile
genCModFile = id

genModFiles :: MFCompiler r -> r -> FilePath -> [Filename] -> IO ModFiles
genModFiles mfc env fp excludes = do
  fortranFiles <- fmap fst <$> readParseSrcDir emptyModFiles fp excludes
  traverse (genCModFile mfc env emptyModFiles) fortranFiles

-- | Retrieve the ModFiles under a given path.
getModFiles :: FilePath -> IO ModFiles
getModFiles dir = do
  -- Figure out the camfort mod files and parse them.
  modFileNames <- filter isModFile <$> listDirectoryRecursively dir
  mods <- forM modFileNames $ \ modFileName -> do
    modData <- B.readFile (dir </> modFileName)
    let eResult = decodeModFile modData
    case eResult of
      Left msg -> do
        putStrLn $ modFileName ++ ": Error: " ++ show msg
        pure Nothing
      Right modFile -> do
        putStrLn $ modFileName ++ ": successfully parsed precompiled file."
        pure . pure $ modFile
  pure . catMaybes $ mods
  where
    isModFile :: String -> Bool
    isModFile = (== modFileSuffix) . takeExtension

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

readParseSrcDir :: ModFiles
                -> FileOrDir
                -> [Filename]
                -> IO [(F.ProgramFile A, SourceText)]
readParseSrcDir mods inp excludes = do
  isdir <- isDirectory inp
  files <-
    if isdir
    then do
      files <- getFortranFiles inp
      -- Compute alternate list of excludes with the
      -- the directory appended
      let excludes' = excludes ++ map (\x -> inp </> x) excludes
      pure $ map (\y -> inp </> y) files \\ excludes'
    else pure [inp]
  mapMaybeM (readParseSrcFile mods) files
  where
    mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
    mapMaybeM f = fmap catMaybes . mapM f

readParseSrcFile :: ModFiles -> Filename -> IO (Maybe (F.ProgramFile A, SourceText))
readParseSrcFile mods f = do
  inp <- flexReadFile f
  let result = FP.fortranParserWithModFiles mods inp f
  case result of
    Right ast -> pure $ Just (fmap (const unitAnnotation) ast, inp)
    Left  err -> print err >> pure Nothing
  where
    -- | Read file using ByteString library and deal with any weird characters.
    flexReadFile :: String -> IO B.ByteString
    flexReadFile = fmap (encodeUtf8 . decodeUtf8With (replace ' ')) . B.readFile

getFortranFiles :: FileOrDir -> IO [String]
getFortranFiles dir =
  filter isFortran <$> listDirectoryRecursively dir
  where
    -- | True if the file has a valid fortran extension.
    isFortran :: Filename -> Bool
    isFortran x = takeExtension x `elem` (exts ++ extsUpper)
      where exts = [".f", ".f90", ".f77", ".cmn", ".inc"]
            extsUpper = map (map toUpper) exts
