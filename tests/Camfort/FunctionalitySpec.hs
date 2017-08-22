-- TODO: Fix this
module Camfort.FunctionalitySpec (spec) where

import System.Directory (copyFile, doesDirectoryExist)
import System.FilePath  ((</>))
import System.IO.Silently (capture_)
import System.IO.Temp   (withSystemTempDirectory)

import           Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Camfort.Functionality
  ( AnnotationType(ATDefault)
  , camfortInitialize
  , stencilsInfer
  , unitsCheck, unitsInfer, unitsSynth )
import Camfort.Specification.Units.Monad (LiteralsOpt(LitMixed))

spec :: Test.Spec
spec = do
  describe "camfortInitialize" $ return ()
    -- xit "creates a .camfort directory" $
    --   return ()
      -- withSystemTempDirectory "camfort-test-tmp"
      --   (\d -> camfortInitialize d >> doesDirectoryExist (d </> ".camfort"))
      -- `shouldReturn` True
  xdescribe "units-check" $ return ()
  --   it "correctly detects basic cross-module inconsistent"
  --     return ()
  --     -- unitsCheckTestCrossModuleInconsistBasic
  xdescribe "units-infer" $ return ()
  --   it "shows consistency error with inconsistent program"
  --     return ()
  --     -- unitsInferTestCrossModuleInconsistBasic
  xdescribe "units-synth" $ return ()
  --   it "correct synth output with basic, consistent file"
  --     return ()
  --     -- unitsSynthTestBasic
  xdescribe "stencils-infer" $ return ()
  --   it "correctly infers with basic cross-module"
  --     return ()
      -- stencilsInferTestCrossModuleBasic

analysisWithTmpFilesTest
  :: FilePath -> [FilePath] -> FilePath
  -> (FilePath -> Maybe FilePath -> [t] -> IO a)
  -> (FilePath -> String)
  -> Expectation
analysisWithTmpFilesTest dir files file analysis expected =
  withSystemTempDirectory "camfort-test-tmp"
  (\d -> do
      mapM_ (\fname -> copyFile (dir </> fname) (d </> fname)) files
      let actFile = d </> file
      res <- capture_ $ analysis actFile (Just d) []
      res `shouldBe` expected actFile)

fixturesDir :: FilePath
fixturesDir = "tests" </> "fixtures" </> "Specification"

unitsFixturesDir :: FilePath
unitsFixturesDir = fixturesDir </> "Units"

unitsWithTmpFilesTest
  :: FilePath -> [FilePath] -> FilePath
  -> (FilePath -> Maybe FilePath -> [t] -> LiteralsOpt -> Bool -> IO a)
  -> (FilePath -> String)
  -> Expectation
unitsWithTmpFilesTest dir files file analysis =
  analysisWithTmpFilesTest dir files file analysis'
  where analysis' inSrc incDir excludes = analysis inSrc incDir excludes LitMixed False

basicUnitsCrossModuleTestHelper
  :: (FilePath -> Maybe FilePath -> [t] -> LiteralsOpt -> Bool -> IO a)
  -> (FilePath -> String)
  -> Expectation
basicUnitsCrossModuleTestHelper =
  unitsWithTmpFilesTest unitsFixturesModuleDir
    ["crossmoduleuser.f90", "crossmoduleprovider.f90"] "crossmoduleuser.f90"
  where unitsFixturesModuleDir = unitsFixturesDir </> "cross-module-a"

-- unitsCheckTestCrossModuleInconsistBasic :: Expectation
-- unitsCheckTestCrossModuleInconsistBasic =
--   basicUnitsCrossModuleTestHelper unitsCheck unitsCheckTestCrossModuleInconsistBasicReport

-- unitsInferTestCrossModuleInconsistBasic :: Expectation
-- unitsInferTestCrossModuleInconsistBasic =
--   basicUnitsCrossModuleTestHelper unitsInfer unitsInferTestCrossModuleInconsistBasicReport

-- unitsSynthTestBasic :: Expectation
-- unitsSynthTestBasic =
--   unitsWithTmpFilesTest unitsFixturesDir
--     ["example-simple-1.f90"] "example-simple-1.f90" unitsSynth' unitsSynthBasicReport
--   where unitsSynth' inSrc incDir excludes m debug =
--           unitsSynth inSrc incDir excludes m debug inSrc ATDefault

unitsCheckTestCrossModuleInconsistBasicReport :: FilePath -> String
unitsCheckTestCrossModuleInconsistBasicReport fp =
  "Checking units for '" ++ fp ++ "'\n\n" ++
  fp ++ ": Inconsistent:\n\
  \ - at 7:3: 'literal' should have unit 'm'\n\
  \ - at 7:3: 'parameter 1 to add' should have unit 'm'\n\
  \ - at 8:3: 'literal' should have unit 's'\n\
  \ - at 9:3: 'z' should have the same units as 'result of add'\n\n"

unitsInferTestCrossModuleInconsistBasicReport :: FilePath -> String
unitsInferTestCrossModuleInconsistBasicReport fp =
  "Inferring units for '" ++ fp ++ "'\n\n" ++
  fp ++ ": Inconsistent:\n\
  \ - at 7:3: 'literal' should have unit 'm'\n\
  \ - at 7:3: 'parameter 1 to add' should have unit 'm'\n\
  \ - at 8:3: 'literal' should have unit 's'\n\
  \ - at 9:3: 'z' should have the same units as 'result of add'\n\n"

unitsSynthBasicReport :: FilePath -> String
unitsSynthBasicReport fp =
  "Synthesising units for '" ++ fp ++ "'\n" ++
  "Writing " ++ fp ++ "\n\n" ++
  fp ++ ":\n" ++
  "  3:14 unit s :: x\n\
  \  3:17 unit s :: y\n\n"

-- stencilsInferTestCrossModuleBasic :: Expectation
-- stencilsInferTestCrossModuleBasic =
--   analysisWithTmpFilesTest stencilsFixturesModuleDir
--     ["user.f90", "provider.f90"] "user.f90" stencilsInfer' stencilsInferCrossModuleBasicReport
--   where stencilsFixturesModuleDir = fixturesDir </> "Stencils" </> "cross-module-a"
--         stencilsInfer' inSrc incDir excludes = stencilsInfer inSrc incDir excludes False

-- stencilsInferCrossModuleBasicReport :: FilePath -> String
-- stencilsInferCrossModuleBasicReport fp =
--   "Inferring stencil specs for '" ++ fp ++ "'\n\n" ++
--   fp ++ "\n(7:6)-(7:16)    stencil readOnce, pointed(dim=1) :: b\n"
