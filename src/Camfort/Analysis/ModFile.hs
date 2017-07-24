{- |
Module      :  Camfort.Analysis.ModFile
Description :  CamFort-specific ModFiles helpers.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

module Camfort.Analysis.ModFile
  ( getModFiles
  ) where

import           Control.Monad (forM)
import qualified Data.ByteString as B
import           Data.Maybe (catMaybes)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath ((</>), takeExtension)

import Language.Fortran.Util.ModFile

-- | Retrieve the ModFiles under a given path.
getModFiles :: FilePath -> IO ModFiles
getModFiles dir = do
  -- Figure out the camfort mod files and parse them.
  modFileNames <- filter isModFile <$> listDirectoryRec dir ""
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
    listDirectoryRec :: FilePath -> FilePath -> IO [FilePath]
    listDirectoryRec d f = do
      let fullPath = d </> f
      isDirectory <- doesDirectoryExist fullPath
      if isDirectory
      then do
        conts <- listDirectory fullPath
        concat <$> mapM (listDirectoryRec fullPath) conts
      else pure [fullPath]
