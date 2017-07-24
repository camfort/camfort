{-# LANGUAGE OverloadedStrings #-}

module Camfort.Analysis.ModFileSpec (spec) where

import Data.Binary (encodeFile)
import System.Directory (createDirectory)
import System.FilePath ((</>), (<.>))
import System.IO.Temp (withSystemTempDirectory)

import Language.Fortran.Util.ModFile

import Camfort.Analysis.ModFile (getModFiles)

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

spec :: Test.Spec
spec =
  describe "getModFiles" $
    it "correctly retrieves ModFiles of arbitrary depth" $
      withSystemTempDirectory "camfort-modfilespec"
        (\dir -> do
          let mkMod name = alterModFileData (const $ Just name) "mfs-name" emptyModFile
              mod1       = mkMod "file-a"
              mod2       = mkMod "file-b"
          encodeFile (dir </> "moda" <.> modFileSuffix) mod1
          createDirectory (dir </> "dir1")
          encodeFile (dir </> "dir1" </> "modb" <.> modFileSuffix) mod2
          fmap (fmap $ lookupModFileData "mfs-name") . getModFiles $ dir)
        `shouldReturn` [Just "file-b", Just "file-a"]
