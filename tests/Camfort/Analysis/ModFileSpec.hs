{-# LANGUAGE OverloadedStrings #-}

module Camfort.Analysis.ModFileSpec (spec) where

import           Camfort.Analysis.ModFile (getModFiles)
import           Data.Binary (encodeFile)
import           Data.List (sort)
import           Language.Fortran.Util.ModFile
import           System.FilePath ((</>), (<.>))
import           System.IO.Temp (withSystemTempDirectory)
import qualified Test.Hspec as Test
import           Test.Hspec hiding (Spec)

spec :: Test.Spec
spec =
  describe "getModFiles" $
    it "correctly retrieves ModFiles" $
      withSystemTempDirectory "camfort-modfilespec"
        (\dir -> do
          let mkMod name = alterModFileData (const $ Just name) "mfs-name" emptyModFile
              mod1       = mkMod "file-a"
              mod2       = mkMod "file-b"
          encodeFile (dir </> "moda" <.> modFileSuffix) [mod1]
          encodeFile (dir </> "modb" <.> modFileSuffix) [mod2]
          fmap (sort . fmap (lookupModFileData "mfs-name")) . getModFiles $ dir)
        `shouldReturn` [Just "file-a", Just "file-b"]
