module Camfort.Specification.Units.Analysis.ConsistentSpec (spec) where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Camfort.Analysis.Fortran (analysisResult)
import Camfort.Analysis.ModFile (getModFiles)
import Camfort.Input (readParseSrcDirWithModFiles)
import Camfort.Specification.Units.Analysis (runUnitsAnalysis)
import Camfort.Specification.Units.Analysis.Consistent (checkUnits)
import Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoDebug, uoLiterals)

spec :: Test.Spec
spec =
  describe "consistency analysis" $ do
    it "reports (simple) inconsistent units" $
       "example-inconsist-1.f90" `unitsCheckReportIs` exampleInconsist1CheckReport
    it "Polymorphic non-zero literal is not OK" $
       "inconsistLitInPolyFun.f90" `unitsCheckReportIs` inconsistLitInPolyFunReport
    it "Recursive Multiplication is not OK" $
       "inconsistRecMult.f90" `unitsCheckReportIs` inconsistRecMultReport
    describe "reports with varying Literal Modes" $ do
      it "LitMixed" $
        unitsCheckReport LitMixed    "inconsist3.f90" inconsist3LitMixedReport
      it "LitPoly" $
        unitsCheckReport LitPoly     "inconsist3.f90" inconsist3LitPolyReport
      it "LitUnitless" $
        unitsCheckReport LitUnitless "inconsist3.f90" inconsist3LitUnitlessReport

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units checking on a file is as expected.
unitsCheckReport :: LiteralsOpt -> String -> String -> Expectation
unitsCheckReport lo fileName expectedReport = do
  let file = fixturesDir </> fileName
  incDir <- getCurrentDirectory
  [(pf,_)] <- readParseSrcDirWithModFiles file incDir []
  modFiles <- getModFiles incDir
  let report = analysisResult $ runUnitsAnalysis checkUnits uOpts modFiles pf
  show report `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoDebug = False, uoLiterals = lo }

-- | Assert that the report of performing units checking on a file is as expected.
unitsCheckReportIs :: String -> String -> Expectation
unitsCheckReportIs = unitsCheckReport LitMixed

exampleInconsist1CheckReport :: String
exampleInconsist1CheckReport =
  "\ntests/fixtures/Specification/Units/example-inconsist-1.f90: Inconsistent:\n\
  \ - at 7:7: 'z' should have unit 's'\n\
  \ - at 7:7: Units 's' and 'm' should be equal\n"

inconsist3LitMixedReport :: String
inconsist3LitMixedReport =
  "\ntests/fixtures/Specification/Units/inconsist3.f90: Inconsistent:\n\
  \ - at 5:7: 'j' should have unit 'literal'\n\
  \ - at 5:7: 'literal' should have unit 'literal'\n\
  \ - at 7:7: 'k' should have the same units as 'j * j'\n"

inconsist3LitPolyReport :: String
inconsist3LitPolyReport =
  "\ntests/fixtures/Specification/Units/inconsist3.f90: Inconsistent:\n\
  \ - at 7:7: 'k' should have the same units as 'j * j'\n"

inconsist3LitUnitlessReport :: String
inconsist3LitUnitlessReport =
  "\ntests/fixtures/Specification/Units/inconsist3.f90: Inconsistent:\n\
  \ - at 5:7: 'j' should have unit '1'\n"

inconsistLitInPolyFunReport :: String
inconsistLitInPolyFunReport =
  "\ntests/fixtures/Specification/Units/inconsistLitInPolyFun.f90: Inconsistent:\n\
  \ - at 10:3: 'a' should have the same units as 'result of sqr'\n\
  \ - at 10:11: 'literal' should have unit 'm'\n\
  \ - at 10:11: 'parameter 1 to sqr' should have unit 'm'\n\
  \ - at 11:11: 'literal' should have unit 's'\n"

inconsistRecMultReport :: String
inconsistRecMultReport =
  "\ntests/fixtures/Specification/Units/inconsistRecMult.f90: Inconsistent:\n\
  \ - at 4:3: 'z' should have the same units as 'result of recur'\n\
  \ - at 4:13: 'x' should have unit 'literal'\n\
  \ - at 4:15: 'literal' should have unit 'm'\n\
  \ - at 9:9: 'x' should have the same units as 'parameter 1 to recur'\n\
  \ - at 10:8: 'parameter 2 to recur' should have unit 'm'\n"
