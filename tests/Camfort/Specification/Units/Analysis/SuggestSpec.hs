module Camfort.Specification.Units.Analysis.SuggestSpec (spec) where

import           Camfort.Analysis hiding (describe)
import           Camfort.Analysis.ModFile (readParseSrcDir, genModFiles)
import           Camfort.Specification.Units.Analysis (compileUnits)
import           Camfort.Specification.Units.Analysis.Criticals (inferCriticalVariables)
import           Camfort.Specification.Units.Monad (LiteralsOpt(..), unitOpts0, UnitOpts, uoLiterals, uninitializeds, UnitEnv(..), runUnitAnalysis)
import           Control.Lens
import           Language.Fortran.Util.ModFile (emptyModFiles)
import           System.FilePath ((</>))
import qualified Test.Hspec as Test
import           Test.Hspec hiding (Spec)
import           Camfort.TestUtils (hideFormatting, normalisedShouldBe)
import           Language.Fortran.Util.ModFile

spec :: Test.Spec
spec = do
  describe "units-suggest" $ do
    it "suggests get_lightspeed for internals.f90" $
       unitsSuggestReportIs LitMixed False [] "internals.f90" expectedInternalsPolyReport
    it "suggests all three variables for simple_function_internal.f90" $
       unitsSuggestReportIs LitMixed False [] "simple_function_internal.f90" expectedSimpleInternalReport

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units suggest on a file is as expected.
unitsSuggestReportIs :: LiteralsOpt -> Bool -> [String] -> String -> String -> Expectation
unitsSuggestReportIs litmode uninitmode modNames fileName expectedReport = do
  let file = fixturesDir </> fileName
      modPaths = fmap (fixturesDir </>) modNames
  modFiles <- mapM (mkTestModFile uOpts) modPaths
  [(pf,_)] <- readParseSrcDir Nothing modFiles file []

  let uEnv = UnitEnv { unitOpts = uOpts, unitProgramFile = pf }

  report <- runAnalysisT file (logOutputNone True) LogError modFiles $ runUnitAnalysis uEnv $ inferCriticalVariables ""
  let res = report ^?! arResult . _ARSuccess

  hideFormatting (show res) `normalisedShouldBe` expectedReport
  where uOpts = unitOpts0 { uoLiterals = litmode, uninitializeds = uninitmode }

-- | Helper for producing a basic ModFile from a (terminal) module file.
mkTestModFile :: UnitOpts -> String -> IO ModFile
mkTestModFile uopts file =
  head <$> genModFiles Nothing emptyModFiles compileUnits uopts file []

expectedInternalsPolyReport :: String
expectedInternalsPolyReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "internals.f90: 1 variable declarations suggested to be given a specification:\n\
  \    tests" </> "fixtures" </> "Specification" </> "Units" </> "internals.f90:9:19    get_lightspeed\n"

expectedSimpleInternalReport :: String
expectedSimpleInternalReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "simple_function_internal.f90: 3 variable declarations suggested to be given a specification:\n\
  \    tests" </> "fixtures" </> "Specification" </> "Units" </> "simple_function_internal.f90:10:21    print_lightspeed\n\
  \    tests" </> "fixtures" </> "Specification" </> "Units" </> "simple_function_internal.f90:12:21    lightspeed\n\
  \    tests" </> "fixtures" </> "Specification" </> "Units" </> "simple_function_internal.f90:13:18    lightspeed_converted\n"
