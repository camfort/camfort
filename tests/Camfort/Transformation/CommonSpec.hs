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
  [ Example "toArgs.f90" "toArgs.expected.f90"
  , Example "toArgs2.f90" "toArgs2.expected.f90"
  ]

readExpected :: FilePath -> IO String
readExpected filename = do
  let path = samplesBase </> filename
  readFile path

readActual :: FilePath -> IO String
readActual argumentFilename = do
  let argumentPath = samplesBase </> argumentFilename
  let outFile = argumentPath `addExtension` "out"
  commonToArgs argumentPath [] outFile ()
  actual <- readFile outFile
  removeFile outFile
  return actual

spec :: Spec
spec =
  describe "Issue #9" $
    context "lalala" $ do
      expected <- runIO $ readExpected "toArgs.expected.f90"
      actual <- runIO $ readActual "toArgs.f90"
      it "it eliminates common statement" $
        actual `shouldBe` expected
