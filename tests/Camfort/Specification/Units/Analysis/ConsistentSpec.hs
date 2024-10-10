-- TODO: Fix this
module Camfort.Specification.Units.Analysis.ConsistentSpec (spec) where

import           Camfort.Analysis hiding (describe)
import           Camfort.Analysis.ModFile (genModFiles)
import           Camfort.Input (readParseSrcDir)
import           Camfort.Specification.Units.Analysis (compileUnits)
import           Camfort.Specification.Units.Analysis.Consistent (checkUnits)
import           Camfort.Specification.Units.Monad (LiteralsOpt(..), unitOpts0, uoLiterals, runUnitAnalysis, UnitEnv(..))
import           Control.Lens
import           Language.Fortran.Util.ModFile (ModFile, emptyModFiles)
import           System.FilePath ((</>))
import qualified Test.Hspec as Test
import           Test.Hspec hiding (Spec)
import           Camfort.TestUtils

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
        unitsCheckReportNoMod LitMixed    "inconsist3.f90" inconsist3LitMixedReport
      it "LitPoly" $
        unitsCheckReportNoMod LitPoly     "inconsist3.f90" inconsist3LitPolyReport
      it "LitUnitless" $
        unitsCheckReportNoMod LitUnitless "inconsist3.f90" inconsist3LitUnitlessReport
    describe "cross-module" $
      it "basic inconsistent" $
        unitsCheckReportWithMod ["cross-module-a/crossmoduleprovider.f90"] "cross-module-a/crossmoduleuser.f90"
          crossModuleInconsistBasicReport
    describe "literals" $ do
      it "nonzero literal with explicitly annotated polymorphic units-variable" $
        "literal-nonzero-inconsist1.f90" `unitsCheckReportIs` literalNonZeroInconsist1Report
      it "nonzero literal is unitless in poly-context" $
        "literal-nonzero-inconsist2.f90" `unitsCheckReportIs` literalNonZeroInconsist2Report
      it "monomorphism restriction is important (do-loop)" $
        "literal-nonzero-inconsist3.f90" `unitsCheckReportIs` literalNonZeroInconsist3Report
      -- it "monomorphism restriction is important (do-loop with zero-start)" $
      --   "literal-nonzero-inconsist4.f90" `unitsCheckReportIs` literalNonZeroInconsist4Report
      it "monomorphism restriction is important (goto)" $
        "literal-nonzero-inconsist5.f90" `unitsCheckReportIs` literalNonZeroInconsist5Report


fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units checking on a file is as expected.
unitsCheckReport :: LiteralsOpt -> [String] -> String -> String -> Expectation
unitsCheckReport lo modNames fileName expectedReport = do
  let file = fixturesDir </> fileName
      modPaths = fmap (fixturesDir </>) modNames
  modFiles <- mapM mkTestModFile modPaths
  [(pf,_)] <- readParseSrcDir Nothing modFiles file []

  let uEnv = UnitEnv { unitOpts = uOpts, unitProgramFile = pf }

  report <- runAnalysisT file (logOutputNone True) LogError modFiles $ runUnitAnalysis uEnv $ checkUnits
  let res = report ^?! arResult . _ARSuccess

  hideFormatting (show res) `shouldBe` expectedReport
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
mkTestModFile file = head <$> genModFiles Nothing emptyModFiles compileUnits unitOpts0 file []

exampleInconsist1CheckReport :: String
exampleInconsist1CheckReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "example-inconsist-1.f90: Inconsistent:\n\
  \ - at 7:7: Units 's' and 'm' should be equal\n"

inconsist3LitMixedReport :: String
inconsist3LitMixedReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "inconsist3.f90: Inconsistent:\n\
  \ - at 6:3: 'j**2' should have the same units as 'k'\n"

inconsist3LitPolyReport :: String
inconsist3LitPolyReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "inconsist3.f90: Inconsistent:\n\
   \ - at 6:3: 'j**2' should have the same units as 'k'\n"

inconsist3LitUnitlessReport :: String
inconsist3LitUnitlessReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "inconsist3.f90: Inconsistent:\n\
  \ - at 6:3: 'j**2' should have the same units as 'k'\n"

inconsistLitInPolyFunReport :: String
inconsistLitInPolyFunReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "inconsistLitInPolyFun.f90: Inconsistent:\n\
  \ - at 10:3: 'a' should have the same units as 'result of sqr'\n\
  \ - at 10:11: 'parameter 1 to sqr' should have unit 'm'\n"

inconsistRecMultReport :: String
inconsistRecMultReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "inconsistRecMult.f90: Inconsistent:\n\
  \ - at 4:15: 'parameter 2 to recur' should have unit 'm'\n"

crossModuleInconsistBasicReport :: String
crossModuleInconsistBasicReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-a" </> "crossmoduleuser.f90: Inconsistent:\n\
  \ - at 9:11: 'parameter 1 to add' should have unit 'm'\n"

literalNonZeroInconsist1Report :: String
literalNonZeroInconsist1Report =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero-inconsist1.f90: Inconsistent:\n\
  \'literal number' should have unit '1'\n"

literalNonZeroInconsist2Report :: String
literalNonZeroInconsist2Report =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero-inconsist2.f90: Inconsistent:\n\
  \ - at 6:3: 'b' should have the same units as 'result of f'\n\
  \ - at 6:9: 'parameter 1 to f' should have unit 'm'\n"

literalNonZeroInconsist3Report :: String
literalNonZeroInconsist3Report =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero-inconsist3.f90: Inconsistent:\n\
  \ - at 6:3: 'result of sqr' should have unit 'm'\n\
  \ - at 6:11: 'parameter 1 to sqr' should have unit 'm'\n\
  \ - at 7:3: 'result of sqr' should have unit 's'\n\
  \ - at 7:11: 'parameter 1 to sqr' should have unit 's'\n"

-- literalNonZeroInconsist4Report :: String -- fixme
-- literalNonZeroInconsist4Report =
--   "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero-inconsist5.f90: Inconsistent:\n\
--   \- at 7:11: 'parameter 1 to sqr' should have unit 's'\n\
--   \- at 15:9: 'i' should have the same units as 'parameter 1 to sqr'\n\
--   \- at 17:12: 'i' should have unit '1'\n"

literalNonZeroInconsist5Report :: String
literalNonZeroInconsist5Report =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero-inconsist5.f90: Inconsistent:\n\
  \ - at 6:3: 'result of sqr' should have unit 'm'\n\
  \ - at 6:11: 'parameter 1 to sqr' should have unit 'm'\n"
