module Camfort.Specification.Units.Analysis.CriticalsSpec (spec) where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Camfort.Analysis.Fortran (analysisResult, runAnalysis)
import Camfort.Analysis.ModFile (getModFiles)
import Camfort.Input (readParseSrcDirWithModFiles)
import Camfort.Specification.Units.Analysis.Criticals (inferCriticalVariables)
import Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoDebug, uoLiterals)

spec :: Test.Spec
spec = do
  describe "critical-units analysis" $ do
    it "reports critical variables" $
       "example-criticals-1.f90" `unitsCriticalsReportIs` exampleCriticals1CriticalsReport
    it "reports when no additional variables need to be annotated" $
       "example-criticals-2.f90" `unitsCriticalsReportIs` exampleCriticals2CriticalsReport

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units inference on a file is as expected.
unitsCriticalsReportIs :: String -> String -> Expectation
unitsCriticalsReportIs fileName expectedReport = do
  let file = fixturesDir </> fileName
  incDir <- getCurrentDirectory
  [(pf,_)] <- readParseSrcDirWithModFiles file incDir []
  modFiles <- getModFiles incDir
  let report = analysisResult $ runAnalysis (inferCriticalVariables uOpts) modFiles pf
  show report `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoDebug = False, uoLiterals = LitMixed }

exampleCriticals1CriticalsReport :: String
exampleCriticals1CriticalsReport =
  "\ntests/fixtures/Specification/Units/example-criticals-1.f90: 2 variable declarations suggested to be given a specification:\n\
  \    tests/fixtures/Specification/Units/example-criticals-1.f90 (3:17)    b\n\
  \    tests/fixtures/Specification/Units/example-criticals-1.f90 (3:20)    c\n"

exampleCriticals2CriticalsReport :: String
exampleCriticals2CriticalsReport =
  "\ntests/fixtures/Specification/Units/example-criticals-2.f90: No additional annotations are necessary.\n"
