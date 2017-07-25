module Camfort.Specification.UnitsSpec (spec) where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import Test.Hspec

import Camfort.Analysis.Fortran (analysisResult, runAnalysis)
import Camfort.Analysis.ModFile (getModFiles)
import Camfort.Input (FileProgram, readParseSrcDirWithModFiles)
import Camfort.Specification.Units
  (checkUnits, inferCriticalVariables, inferUnits)
import Camfort.Specification.Units.Analysis (UnitsAnalysis)
import Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoDebug, uoLiterals)

spec :: Spec
spec =
  describe "fixtures integration tests" $ do
    describe "units-suggest" $ do
      it "reports critical variables" $
         "example-criticals-1.f90" `unitsCriticalsReportIs` exampleCriticals1CriticalsReport
      it "reports when no additional variables need to be annotated" $
         "example-criticals-2.f90" `unitsCriticalsReportIs` exampleCriticals2CriticalsReport
    describe "units-check" $
      it "reports (simple) inconsistent units" $
         "example-inconsist-1.f90" `unitsCheckReportIs` exampleInconsist1CheckReport
    describe "units-infer" $
      it "infers correctly based on simple addition" $
         "example-simple-1.f90" `unitsInferReportIs` exampleInferSimple1Report

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

unitsAnalysisTest :: (Show b) => UnitsAnalysis FileProgram a -> (a -> b) -> String -> String -> Expectation
unitsAnalysisTest analysis normF fileName expectedReport = do
  let file = fixturesDir </> fileName
  incDir <- getCurrentDirectory
  [(pf,_)] <- readParseSrcDirWithModFiles file incDir []
  modFiles <- getModFiles incDir
  let report = normF . analysisResult $ runAnalysis (analysis uOpts) modFiles pf
  show report `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoDebug = False, uoLiterals = LitMixed }

-- | Assert that the report of performing units checking on a file is as expected.
unitsCheckReportIs :: String -> String -> Expectation
unitsCheckReportIs = unitsAnalysisTest checkUnits id

-- | Assert that the report of performing units inference on a file is as expected.
unitsInferReportIs :: String -> String -> Expectation
unitsInferReportIs = unitsAnalysisTest inferUnits (\(Right r) -> r)

-- | Assert that the report of performing units inference on a file is as expected.
unitsCriticalsReportIs :: String -> String -> Expectation
unitsCriticalsReportIs = unitsAnalysisTest inferCriticalVariables id

exampleCriticals1CriticalsReport :: String
exampleCriticals1CriticalsReport =
  "\ntests/fixtures/Specification/Units/example-criticals-1.f90: 2 variable declarations suggested to be given a specification:\n\
  \    tests/fixtures/Specification/Units/example-criticals-1.f90 (3:17)    b\n\
  \    tests/fixtures/Specification/Units/example-criticals-1.f90 (3:20)    c\n"

exampleCriticals2CriticalsReport :: String
exampleCriticals2CriticalsReport =
  "\ntests/fixtures/Specification/Units/example-criticals-2.f90: No additional annotations are necessary.\n"

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
