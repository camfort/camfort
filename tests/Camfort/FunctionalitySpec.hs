module Camfort.FunctionalitySpec (spec) where

import System.Directory (copyFile, doesDirectoryExist)
import System.FilePath  ((</>))
import System.IO.Silently (capture_)
import System.IO.Temp   (withSystemTempDirectory)

import           Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Camfort.Functionality
  (camfortInitialize, stencilsInfer, unitsCheck, unitsInfer)
import Camfort.Specification.Units.Monad (LiteralsOpt(LitMixed))

spec :: Test.Spec
spec = do
  describe "camfortInitialize" $
    it "creates a .camfort directory" $
      withSystemTempDirectory "camfort-test-tmp"
        (\d -> camfortInitialize d >> doesDirectoryExist (d </> ".camfort"))
      `shouldReturn` True
  describe "units-check" $
    it "correctly detects basic cross-module inconsistent"
      unitsCheckTestCrossModuleInconsistBasic
  describe "units-infer" $
    it "shows consistency error with inconsistent program"
      unitsInferTestCrossModuleInconsistBasic
  describe "stencils-infer" $
    it "correctly infers with basic cross-module" $
      stencilsInferTestCrossModuleBasic `shouldReturn` stencilsInferCrossModuleBasicReport

basicUnitsCrossModuleTestHelper
  :: (FilePath -> Maybe FilePath -> [t] -> LiteralsOpt -> Bool -> IO a)
  -> (FilePath -> String)
  -> Expectation
basicUnitsCrossModuleTestHelper analysis expected =
  withSystemTempDirectory "camfort-test-tmp"
  (\d -> do
      let newUserFile = d </> "crossmoduleuser.f90"
      copyFile (unitsFixturesModuleDir </> "crossmoduleuser.f90") newUserFile
      copyFile (unitsFixturesModuleDir </> "crossmoduleprovider.f90") (d </> "crossmoduleprovider.f90")
      res <- capture_ (analysis (d </> "crossmoduleuser.f90") (Just d) [] LitMixed False)
      res `shouldBe` expected newUserFile)
  where unitsFixturesModuleDir =
          "tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-a"

unitsCheckTestCrossModuleInconsistBasic :: Expectation
unitsCheckTestCrossModuleInconsistBasic =
  basicUnitsCrossModuleTestHelper unitsCheck unitsCheckTestCrossModuleInconsistBasicReport

unitsInferTestCrossModuleInconsistBasic :: Expectation
unitsInferTestCrossModuleInconsistBasic =
  basicUnitsCrossModuleTestHelper unitsInfer unitsInferTestCrossModuleInconsistBasicReport

unitsCheckTestCrossModuleInconsistBasicReport :: FilePath -> String
unitsCheckTestCrossModuleInconsistBasicReport fp =
  "Checking units for '" ++ fp ++ "'\n\n" ++
  fp ++ ": Inconsistent:\n\
  \ - at 7:3: 'literal' should have unit 'm'\n\
  \ - at 7:3: 'parameter 1 to add' should have unit 'm'\n\
  \ - at 8:3: 'literal' should have unit 's'\n\
  \ - at 9:3: 'z' should have the same units as 'result of add'\n\n"

unitsInferTestCrossModuleInconsistBasicReport :: FilePath -> String
unitsInferTestCrossModuleInconsistBasicReport fp =
  "Inferring units for '" ++ fp ++ "'\n\n" ++
  fp ++ ": Inconsistent:\n\
  \ - at 7:3: 'literal' should have unit 'm'\n\
  \ - at 7:3: 'parameter 1 to add' should have unit 'm'\n\
  \ - at 8:3: 'literal' should have unit 's'\n\
  \ - at 9:3: 'z' should have the same units as 'result of add'\n\n"

stencilsInferTestCrossModuleBasic :: IO String
stencilsInferTestCrossModuleBasic =
  withSystemTempDirectory "camfort-test-tmp"
  (\d -> do
      copyFile (stencilsFixturesModuleDir </> "user.f90") (d </> "user.f90")
      copyFile (stencilsFixturesModuleDir </> "provider.f90") (d </> "provider.f90")
      (unlines . tail . tail . tail . lines) <$> capture_ (stencilsInfer (d </> "user.f90") (Just d) [] False))
  where stencilsFixturesModuleDir =
          "tests" </> "fixtures" </> "Specification" </> "Stencils" </> "cross-module-a"

stencilsInferCrossModuleBasicReport :: String
stencilsInferCrossModuleBasicReport =
  "(7:6)-(7:16)    stencil readOnce, pointed(dim=1) :: b\n"
