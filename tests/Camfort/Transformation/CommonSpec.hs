module Camfort.Transformation.CommonSpec (spec) where

import System.FilePath
import System.Directory

import Test.Hspec

import Camfort.Helpers
import Camfort.Functionality

samplesBase :: FilePath
samplesBase = "tests" </> "Camfort" </> "Transformation" </> "samples"

data Example = Example FilePath FilePath

examples =
  [ Example "common.f90" "common.expected.f90" ]

readExpected :: FilePath -> IO String
readExpected filename = do
  let path = samplesBase </> filename
  readFile path

readActual :: FilePath -> IO String
readActual argumentFilename = do
  let argumentPath = samplesBase </> argumentFilename
  let outFile = argumentPath `addExtension` "out"
  common argumentPath [] outFile ()
  actual <- readFile outFile
  removeFile outFile
  return actual

spec :: Spec
spec =
  describe "Common block integration test" $
    context "common.f90 into common.expect.f90 and foo.f90" $ do
      expected <- runIO $ readExpected "toArgs.expected.f90"
      actual <- runIO $ readActual "toArgs.f90"
      actualMod <- runIO $ readExpected "foo.f90"
      expectedMod <- runIO $ readExpected "foo.expected.f90"
      it "it eliminates common statement" $
        actual `shouldBe` expected
      it "it produces a correct module file" $
        actualMod `shouldBe` expectedMod
