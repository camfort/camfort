module Camfort.Specification.Units.Analysis.CriticalsSpec (spec) where

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
  describe "critical-units analysis" $ do
    it "reports critical variables" $
       unitsCriticalsReportIs LitMixed False [] "example-criticals-1.f90" exampleCriticals1CriticalsReport
    it "reports when no additional variables need to be annotated" $
       unitsCriticalsReportIs LitMixed False [] "example-criticals-2.f90" exampleCriticals2CriticalsReport

    it "criticals with uninitialized variables included" $
       unitsCriticalsReportIs LitMixed True [] "uninitialised.f90" exampleCriticalUninitMixed

    it "criticals with uninitialized variables included" $
       unitsCriticalsReportIs LitPoly True [] "uninitialised.f90" exampleCriticalUninit


    it "reports correct locales across modules" $ do
       unitsCriticalsReportIs LitPoly False ["cross-module-c" </> "cross-module-c1.f90"]
            ("cross-module-c" </> "cross-module-c3.f90") exampleCriticals3CriticalsReport
    it "reports correct locales across modules and in functions" $ do
       unitsCriticalsReportIs LitPoly False ["cross-module-c" </> "cross-module-c1.f90"]
            ("cross-module-c" </> "cross-module-c1.f90") exampleCriticals4CriticalsReport

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units inference on a file is as expected.
unitsCriticalsReportIs :: LiteralsOpt -> Bool -> [String] -> String -> String -> Expectation
unitsCriticalsReportIs litmode uninitmode modNames fileName expectedReport = do
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

exampleCriticals1CriticalsReport :: String
exampleCriticals1CriticalsReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "example-criticals-1.f90: 2 variable declarations suggested to be given a specification:\n\
  \    tests" </> "fixtures" </> "Specification" </> "Units" </> "example-criticals-1.f90:3:16    b\n\
  \    tests" </> "fixtures" </> "Specification" </> "Units" </> "example-criticals-1.f90:3:19    c\n"

exampleCriticals2CriticalsReport :: String
exampleCriticals2CriticalsReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "example-criticals-2.f90: No additional annotations are necessary.\n"

exampleCriticals3CriticalsReport :: String
exampleCriticals3CriticalsReport =
 "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c3.f90: 6 variable declarations suggested to be given a specification:\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c1.f90:7:10    b\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c1.f90:13:12    d\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c3.f90:5:10    a3\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c3.f90:9:10    b3\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c3.f90:11:10    x0\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c3.f90:12:12    x1\n"

exampleCriticals4CriticalsReport :: String
exampleCriticals4CriticalsReport =
 "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c1.f90: 7 variable declarations suggested to be given a specification:\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c1.f90:5:16    a\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c1.f90:7:10    b\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c1.f90:11:12    foo_out\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c1.f90:18:12    foo3\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c1.f90:24:14    x\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c1.f90:30:12    foo5\n\
 \    tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-c" </> "cross-module-c1.f90:31:12    x\n"

exampleCriticalUninitMixed :: String
exampleCriticalUninitMixed =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "uninitialised.f90: 1 variable declarations suggested to be given a specification:\n\
\    tests" </> "fixtures" </> "Specification" </> "Units" </> "uninitialised.f90:5:12    not_initialised\n"

exampleCriticalUninit :: String
exampleCriticalUninit =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "uninitialised.f90: 2 variable declarations suggested to be given a specification:\n\
\    tests" </> "fixtures" </> "Specification" </> "Units" </> "uninitialised.f90:7:12    initialised\n\
\    tests" </> "fixtures" </> "Specification" </> "Units" </> "uninitialised.f90:5:12    not_initialised\n"