module Camfort.Specification.UnitsSpec (spec) where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import Test.Hspec

import Camfort.Analysis.Fortran (analysisResult)
import Camfort.Analysis.ModFile (getModFiles)
import Camfort.Input (readParseSrcDirWithModFiles)
import Camfort.Specification.Units (inferUnits)
import Camfort.Specification.Units.Analysis (runUnitsAnalysis)
import Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoDebug, uoLiterals)

spec :: Spec
spec =
  describe "fixtures integration tests" $ do
    describe "units-infer" $
      it "infers correctly based on simple addition" $
         "example-simple-1.f90" `unitsInferReportIs` exampleInferSimple1Report

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units inference on a file is as expected.
unitsInferReportIs :: String -> String -> Expectation
unitsInferReportIs fileName expectedReport = do
  let file = fixturesDir </> fileName
  incDir <- getCurrentDirectory
  [(pf,_)] <- readParseSrcDirWithModFiles file incDir []
  modFiles <- getModFiles incDir
  let (Right report) = analysisResult $ runUnitsAnalysis inferUnits uOpts modFiles pf
  show report `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoDebug = False, uoLiterals = LitMixed }

exampleInferSimple1Report :: String
exampleInferSimple1Report =
  "\ntests/fixtures/Specification/Units/example-simple-1.f90:\n\
  \  3:14 unit s :: x\n\
  \  3:17 unit s :: y\n"
