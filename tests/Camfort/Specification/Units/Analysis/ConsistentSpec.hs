-- TODO: Fix this
module Camfort.Specification.Units.Analysis.ConsistentSpec (spec) where

import System.FilePath ((</>))

import Control.Lens

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Language.Fortran.Util.ModFile (ModFile, emptyModFiles)

import Camfort.Analysis hiding (describe)
import Camfort.Analysis.ModFile (genModFiles)
import Camfort.Input (readParseSrcDir)
import Camfort.Specification.Units.Analysis (compileUnits)
import Camfort.Specification.Units.Analysis.Consistent (checkUnits)
import Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoLiterals, runUnitAnalysis, UnitEnv(..))

spec :: Test.Spec
spec =
  xdescribe "consistency analysis" $ do
    it "reports (simple) inconsistent units" $
       "example-inconsist-1.f90" `unitsCheckReportIs` exampleInconsist1CheckReport
    it "Polymorphic non-zero literal is not OK" $
       "inconsistLitInPolyFun.f90" `unitsCheckReportIs` inconsistLitInPolyFunReport
    it "Recursive Multiplication is not OK" $
       "inconsistRecMult.f90" `unitsCheckReportIs` inconsistRecMultReport
    describe "reports with varying Literal Modes" $ do
      it "LitMixed" $
        unitsCheckReportNoMod LitMixed    "inconsist3.f90" inconsist3LitMixedReport
      it "LitPoly" $
        unitsCheckReportNoMod LitPoly     "inconsist3.f90" inconsist3LitPolyReport
      it "LitUnitless" $
        unitsCheckReportNoMod LitUnitless "inconsist3.f90" inconsist3LitUnitlessReport
    describe "cross-module" $
      it "basic inconsistent" $
        unitsCheckReportWithMod ["cross-module-a/crossmoduleprovider.f90"] "cross-module-a/crossmoduleuser.f90"
          crossModuleInconsistBasicReport

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units checking on a file is as expected.
unitsCheckReport :: LiteralsOpt -> [String] -> String -> String -> Expectation
unitsCheckReport lo modNames fileName expectedReport = do
  let file = fixturesDir </> fileName
      modPaths = fmap (fixturesDir </>) modNames
  modFiles <- mapM mkTestModFile modPaths
  [(pf,_)] <- readParseSrcDir modFiles file []

  let uEnv = UnitEnv { unitOpts = uOpts, unitProgramFile = pf }

  report <- runAnalysisT file (logOutputNone True) LogError modFiles $ runUnitAnalysis uEnv $ checkUnits
  let res = report ^?! arResult . _ARSuccess

  show res `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoLiterals = lo }

unitsCheckReportWithMod :: [String] -> String -> String -> Expectation
unitsCheckReportWithMod = unitsCheckReport LitMixed

unitsCheckReportNoMod :: LiteralsOpt -> String -> String -> Expectation
unitsCheckReportNoMod lo = unitsCheckReport lo []

-- | Assert that the report of performing units checking on a file is as expected.
unitsCheckReportIs :: String -> String -> Expectation
unitsCheckReportIs = unitsCheckReport LitMixed []

-- | Helper for producing a basic ModFile from a (terminal) module file.
mkTestModFile :: String -> IO ModFile
mkTestModFile file = head <$> genModFiles compileUnits unitOpts0 file []

exampleInconsist1CheckReport :: String
exampleInconsist1CheckReport =
  "\ntests/fixtures/Specification/Units/example-inconsist-1.f90: Inconsistent:\n\
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

crossModuleInconsistBasicReport :: String
crossModuleInconsistBasicReport =
  "\ntests/fixtures/Specification/Units/cross-module-a/crossmoduleuser.f90: Inconsistent:\n\
  \ - at 7:3: 'literal' should have unit 'm'\n\
  \ - at 7:3: 'parameter 1 to add' should have unit 'm'\n\
  \ - at 8:3: 'literal' should have unit 's'\n\
  \ - at 9:3: 'z' should have the same units as 'result of add'\n"
