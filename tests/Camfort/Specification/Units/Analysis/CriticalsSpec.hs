module Camfort.Specification.Units.Analysis.CriticalsSpec (spec) where

import           Camfort.Analysis hiding (describe)
import           Camfort.Analysis.ModFile (readParseSrcDir, genModFiles)
import           Camfort.Specification.Units.Analysis (compileUnits)
import           Camfort.Specification.Units.Analysis.Criticals (inferCriticalVariables)
import           Camfort.Specification.Units.Monad (LiteralsOpt(..), unitOpts0, uoLiterals, UnitEnv(..), runUnitAnalysis)
import           Control.Lens
import           Language.Fortran.Util.ModFile (emptyModFiles)
import           System.FilePath ((</>))
import qualified Test.Hspec as Test
import           Test.Hspec hiding (Spec)
import           Camfort.TestUtils
import           Language.Fortran.Util.ModFile

spec :: Test.Spec
spec = do
  describe "critical-units analysis" $ do
    it "reports critical variables" $
       unitsCriticalsReportIs LitMixed [] "example-criticals-1.f90" exampleCriticals1CriticalsReport
    it "reports when no additional variables need to be annotated" $
       unitsCriticalsReportIs LitMixed [] "example-criticals-2.f90" exampleCriticals2CriticalsReport
    it "reports correct locales across modules" $ do
       unitsCriticalsReportIs LitPoly ["cross-module-c" </> "cross-module-c1.f90"]
            ("cross-module-c" </> "cross-module-c3.f90") exampleCriticals3CriticalsReport

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units inference on a file is as expected.
unitsCriticalsReportIs :: LiteralsOpt -> [String] -> String -> String -> Expectation
unitsCriticalsReportIs litmode modNames fileName expectedReport = do
  let file = fixturesDir </> fileName
      modPaths = fmap (fixturesDir </>) modNames
  modFiles <- mapM mkTestModFile modPaths
  [(pf,_)] <- readParseSrcDir Nothing modFiles file []

  let uEnv = UnitEnv { unitOpts = uOpts, unitProgramFile = pf }

  report <- runAnalysisT file (logOutputNone True) LogError modFiles $ runUnitAnalysis uEnv $ inferCriticalVariables
  let res = report ^?! arResult . _ARSuccess

  hideFormatting (show res) `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoLiterals = litmode }

-- | Helper for producing a basic ModFile from a (terminal) module file.
mkTestModFile :: String -> IO ModFile
mkTestModFile file = head <$> genModFiles Nothing emptyModFiles compileUnits unitOpts0 file []

exampleCriticals1CriticalsReport :: String
exampleCriticals1CriticalsReport =
  "\ntests/fixtures/Specification/Units/example-criticals-1.f90: 2 variable declarations suggested to be given a specification:\n\
  \    tests/fixtures/Specification/Units/example-criticals-1.f90 (3:17)    b\n\
  \    tests/fixtures/Specification/Units/example-criticals-1.f90 (3:20)    c\n"

exampleCriticals2CriticalsReport :: String
exampleCriticals2CriticalsReport =
  "\ntests/fixtures/Specification/Units/example-criticals-2.f90: No additional annotations are necessary.\n"

exampleCriticals3CriticalsReport :: String
exampleCriticals3CriticalsReport =
 "\ntests/fixtures/Specification/Units/cross-module-c/cross-module-c3.f90: 3 variable declarations suggested to be given a specification:\n\
 \    tests/fixtures/Specification/Units/cross-module-c/cross-module-c1.f90 (7:11)    b\n\
 \    tests/fixtures/Specification/Units/cross-module-c/cross-module-c3.f90 (5:11)    a3\n\
 \    tests/fixtures/Specification/Units/cross-module-c/cross-module-c3.f90 (9:11)    b3\n"