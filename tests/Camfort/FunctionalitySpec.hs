module Camfort.FunctionalitySpec (spec) where

import System.Directory (copyFile, doesDirectoryExist)
import System.FilePath  ((</>))
import System.IO.Silently (capture_)
import System.IO.Temp   (withSystemTempDirectory)

import           Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Camfort.Functionality (camfortInitialize, unitsCheck)
import Camfort.Specification.Units.Monad (LiteralsOpt(LitMixed))

spec :: Test.Spec
spec = do
  describe "camfortInitialize" $
    it "creates a .camfort directory" $
      withSystemTempDirectory "camfort-test-tmp"
        (\d -> (camfortInitialize d >> doesDirectoryExist (d </> ".camfort")))
      `shouldReturn` True
  describe "units-check" $
    it "correctly detects basic cross-module inconsistent" $
      unitsCheckTestCrossModuleInconsistBasic `shouldReturn` unitsCheckTestCrossModuleInconsistBasicReport

unitsCheckTestCrossModuleInconsistBasic :: IO String
unitsCheckTestCrossModuleInconsistBasic =
  withSystemTempDirectory "camfort-test-tmp"
  (\d -> do
      copyFile (unitsFixturesModuleDir </> "crossmoduleuser.f90") (d </> "crossmoduleuser.f90")
      copyFile (unitsFixturesModuleDir </> "crossmoduleprovider.f90") (d </> "crossmoduleprovider.f90")
      (unlines . tail. tail . tail . lines) <$> capture_ (unitsCheck (d </> "crossmoduleuser.f90") (Just d) [] LitMixed False))
  where unitsFixturesModuleDir =
          "tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-a"

unitsCheckTestCrossModuleInconsistBasicReport :: String
unitsCheckTestCrossModuleInconsistBasicReport =
  " - at 7:3: 'literal' should have unit 'm'\n\
  \ - at 7:3: 'parameter 1 to add' should have unit 'm'\n\
  \ - at 8:3: 'literal' should have unit 's'\n\
  \ - at 9:3: 'z' should have the same units as 'result of add'\n\n"
