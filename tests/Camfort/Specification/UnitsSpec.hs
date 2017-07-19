module Camfort.Specification.UnitsSpec (spec) where

import System.FilePath ((</>))

import Test.Hspec

import Camfort.Analysis (runAnalysis)
import Camfort.Input (readParseSrcDir)
import Camfort.Specification.Units (checkUnits)
import Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoDebug, uoLiterals)

spec :: Spec
spec =
  describe "fixtures integration tests" $
    describe "units-check" $
      it "reports (simple) inconsistent units" $
         "example-inconsist-1.f90" `unitsCheckReportIs` exampleInconsist1CheckReport

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units checking on a file is as expected.
unitsCheckReportIs :: String -> String -> Expectation
fileName `unitsCheckReportIs` expectedReport = do
  let file = fixturesDir </> fileName
  [(pf,_)] <- readParseSrcDir file []
  runAnalysis (checkUnits uOpts) pf `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoDebug = False, uoLiterals = LitMixed }

exampleInconsist1CheckReport :: String
exampleInconsist1CheckReport =
  "\ntests/fixtures/Specification/Units/example-inconsist-1.f90: Inconsistent:\n\
  \ - at 7:7: 'z' should have unit 's'\n\
  \ - at 7:7: Units 's' and 'm' should be equal\n\n\n\
  \(7:7)-(7:11): s === m\n\
  \(7:3)-(7:11): unit_of(z) === s\n\
  \(1:1)-(8:19): unit_of(z) === s && s === m\n"
