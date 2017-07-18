module Camfort.FunctionalitySpec (spec) where

import System.Directory (doesDirectoryExist)
import System.FilePath  ((</>))
import System.IO.Temp   (withSystemTempDirectory)

import           Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Camfort.Functionality (camfortInitialize)

spec :: Test.Spec
spec = do
  describe "camfortInitialize" $
    it "creates a .camfort directory" $
      withSystemTempDirectory "camfort-test-tmp"
        (\d -> (camfortInitialize d >> doesDirectoryExist (d </> ".camfort")))
      `shouldReturn` True
