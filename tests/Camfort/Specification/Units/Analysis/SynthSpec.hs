module Camfort.Specification.Units.Analysis.SynthSpec (spec) where

import           Camfort.Analysis hiding (describe)
import           Camfort.Analysis.ModFile (genModFiles, readParseSrcDir)
import           Camfort.Output (refactoring)
import           Camfort.Reprint (reprint)
import           Camfort.Specification.Units (synthesiseUnits)
import           Camfort.Specification.Units.Analysis (compileUnits)
import           Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoLiterals, runUnitAnalysis, UnitEnv(..))
import           Control.Lens
import qualified Data.ByteString.Char8 as B
import           Data.Functor.Identity (runIdentity)
import qualified Language.Fortran.AST as F
import           Language.Fortran.Util.ModFile (ModFile, emptyModFiles)
import           Language.Fortran.Version (deduceFortranVersion)
import           System.FilePath ((</>))
import qualified Test.Hspec as Test
import           Test.Hspec hiding (Spec)

spec :: Test.Spec
spec = do
  describe "units-synth" $ do
    it "synthesises units for array_prog.f90 using array_spec module" $
       unitsSynthReportWithMod ["array_spec.f90"] "array_prog.f90" expectedArrayProgOutput

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units" </> "synth"

-- | Assert that the synthesised output is as expected.
unitsSynthReportWithMod :: [String] -> String -> String -> Expectation
unitsSynthReportWithMod modNames fileName expectedOutput = do
  let file = fixturesDir </> fileName
      modPaths = fmap (fixturesDir </>) modNames
      version = deduceFortranVersion file
  modFiles <- mapM mkTestModFile modPaths
  [(pf, src)] <- readParseSrcDir Nothing modFiles file []

  let uEnv = UnitEnv { unitOpts = uOpts, unitProgramFile = pf }

  report <- runAnalysisT file (logOutputNone True) LogError modFiles $
              runUnitAnalysis uEnv $ synthesiseUnits '='
  let result = report ^?! arResult . _ARSuccess
  case result of
    Left _err -> expectationFailure "Synthesis failed with consistency error"
    Right (_inferReport, pfFinal) -> do
      let output = B.unpack $ runIdentity $ reprint (refactoring version) pfFinal src
      -- Normalise whitespace for comparison
      normaliseOutput output `shouldBe` normaliseOutput expectedOutput
  where
    uOpts = unitOpts0 { uoLiterals = LitMixed }

-- | Helper for producing a basic ModFile from a (terminal) module file.
mkTestModFile :: String -> IO ModFile
mkTestModFile file = head <$> genModFiles Nothing emptyModFiles compileUnits unitOpts0 file []

-- | Normalise output by trimming trailing whitespace from lines and handling CRLF
normaliseOutput :: String -> String
normaliseOutput = unlines . map (reverse . dropWhile (== ' ') . reverse) . lines . filter (/= '\r')

expectedArrayProgOutput :: String
expectedArrayProgOutput = unlines
  [ "program array_prog"
  , ""
  , "    use array_spec"
  , ""
  , "    !=unit(m**3) :: array1"
  , "    real :: array1(array_length) = (/1,2,3,4/)"
  , ""
  , "    !=unit(K) :: array2"
  , "    real :: array2(array_length) = (/1,2,3,4/)"
  , ""
  , "    != unit(k m**3) :: array3"
  , "    real :: array3(array_length) = array1 * array2"
  , ""
  , "end program"
  ]
