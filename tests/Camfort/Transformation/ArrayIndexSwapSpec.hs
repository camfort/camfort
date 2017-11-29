module Camfort.Transformation.ArrayIndexSwapSpec (spec) where

import           System.Directory
import           System.FilePath
import           Test.Hspec

import           Camfort.Analysis.Logger (LogLevel (..))
import           Camfort.Functionality

samplesBase :: FilePath
samplesBase = "tests" </> "fixtures" </> "Transformation"

readSample :: FilePath -> IO String
readSample filename = do
  let path = samplesBase </> filename
  readFile path

removeSample filename = do
  let path = samplesBase </> filename
  removeFile path

spec :: Spec
spec =
  describe "Array index swapping integration test" $
    context "array.f90 into foo.f90" $ do
      expected <- runIO $ readSample "array.expected.f90"
      let outFile = samplesBase </> "foo.f90"
          env = CamfortEnv
            { ceInputSources = samplesBase </> "array.f90"
            , ceIncludeDir = Nothing
            , ceExcludeFiles = []
            , ceLogLevel = LogDebug
            }

      runIO $ arrayIndexSwap 1 2 "a" outFile env

      actual    <- runIO $ readSample "foo.f90"
      runIO $ removeSample "foo.f90"
      it "it swaps the array indices correctly" $
         actual `shouldBe` expected