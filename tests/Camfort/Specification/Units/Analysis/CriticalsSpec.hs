module Camfort.Specification.Units.Analysis.CriticalsSpec (spec) where

import           Camfort.Analysis hiding (describe)
import           Camfort.Analysis.ModFile (readParseSrcDir)
import           Camfort.Specification.Units.Analysis.Criticals (inferCriticalVariables)
import           Camfort.Specification.Units.Monad (LiteralsOpt(..), unitOpts0, uoLiterals, UnitEnv(..), runUnitAnalysis)
import           Control.Lens
import           Language.Fortran.Util.ModFile (emptyModFiles)
import           System.FilePath ((</>))
import qualified Test.Hspec as Test
import           Test.Hspec hiding (Spec)
import           Camfort.TestUtils

spec :: Test.Spec
spec = do
  describe "critical-units analysis" $ do
    it "reports critical variables" $
       "example-criticals-1.f90" `unitsCriticalsReportIs` exampleCriticals1CriticalsReport
    it "reports when no additional variables need to be annotated" $
       "example-criticals-2.f90" `unitsCriticalsReportIs` exampleCriticals2CriticalsReport
    it "reports correct locales across modules" $ do
       ("cross-module" </> "cross-module-c3.f90") `unitsCriticalsReportIs` exampleCriticals3CriticalsReport


fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units inference on a file is as expected.
unitsCriticalsReportIs :: String -> String -> Expectation
unitsCriticalsReportIs fileName expectedReport = do
  let file = fixturesDir </> fileName
      modFiles = emptyModFiles
  [(pf,_)] <- readParseSrcDir Nothing modFiles file []

  let uEnv = UnitEnv { unitOpts = uOpts, unitProgramFile = pf }

  report <- runAnalysisT file (logOutputNone True) LogError modFiles $ runUnitAnalysis uEnv $ inferCriticalVariables
  let res = report ^?! arResult . _ARSuccess

  hideFormatting (show res) `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoLiterals = LitMixed }


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
 "\ntests/fixtures/Specification/Units/cross-module-c3.f90: 3 variable declarations suggested to be given a specification:\
 \    tests/fixtures/Specification/Units/cross-module-c1.f90 (7:11)    b\
 \    tests/fixtures/Specification/Units/cross-module-c3.f90 (5:11)    a3\
 \    tests/fixtures/Specification/Units/cross-module-c3.f90 (9:11)    b3"