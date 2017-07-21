module Camfort.Specification.UnitsSpec (spec) where

import System.FilePath ((</>))

import Test.Hspec

import Camfort.Analysis (runAnalysis)
import Camfort.Input (readParseSrcDir)
import Camfort.Specification.Units (checkUnits, inferUnits)
import Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoDebug, uoLiterals)

spec :: Spec
spec =
  describe "fixtures integration tests" $ do
    describe "units-check" $
      it "reports (simple) inconsistent units" $
         "example-inconsist-1.f90" `unitsCheckReportIs` exampleInconsist1CheckReport
    describe "units-infer" $
      it "infers correctly based on simple addition" $
         "example-simple-1.f90" `unitsInferReportIs` exampleInferSimple1Report

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units checking on a file is as expected.
unitsCheckReportIs :: String -> String -> Expectation
fileName `unitsCheckReportIs` expectedReport = do
  let file = fixturesDir </> fileName
  [(pf,_)] <- readParseSrcDir file []
  let (_, report) = runAnalysis (checkUnits uOpts) pf
  show report `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoDebug = False, uoLiterals = LitMixed }

-- | Assert that the report of performing units inference on a file is as expected.
unitsInferReportIs :: String -> String -> Expectation
fileName `unitsInferReportIs` expectedReport = do
  let file = fixturesDir </> fileName
  [(pf,_)] <- readParseSrcDir file []
  let (_, Right report) = runAnalysis (inferUnits uOpts) pf
  show report `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoDebug = False, uoLiterals = LitMixed }

exampleInconsist1CheckReport :: String
exampleInconsist1CheckReport =
  "\ntests/fixtures/Specification/Units/example-inconsist-1.f90: Inconsistent:\n\
  \ - at 7:7: 'z' should have unit 's'\n\
  \ - at 7:7: Units 's' and 'm' should be equal\n\n\n\
  \(7:7)-(7:11): s === m\n\
  \(7:3)-(7:11): unit_of(z) === s\n\
  \(1:1)-(8:19): unit_of(z) === s && s === m\n"

exampleInferSimple1Report :: String
exampleInferSimple1Report =
  "\ntests/fixtures/Specification/Units/example-simple-1.f90:\n\
  \  3:14 unit s :: x\n\
  \  3:17 unit s :: y\n"
