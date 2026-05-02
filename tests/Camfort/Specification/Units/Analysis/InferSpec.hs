module Camfort.Specification.Units.Analysis.InferSpec (spec) where

import System.FilePath ((</>))
import System.Directory (removeFile, doesFileExist)

import Control.Lens
import Control.Monad (when)

import           Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Language.Fortran.Util.ModFile (ModFile, emptyModFiles)
import Language.Fortran.Version (FortranVersion(Fortran90))

import Camfort.Functionality (CamfortEnv(..),unitsCompile)
import Camfort.Analysis hiding (describe)
import Camfort.Analysis.ModFile (genModFiles, readParseSrcDir)
import Camfort.Specification.Units.Analysis (compileUnits)
import Camfort.Specification.Units.Analysis.Infer (inferUnits)
import Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoLiterals, runUnitAnalysis, UnitEnv(..))
import Camfort.TestUtils (normalisedShouldBe)

spec :: Test.Spec
spec =
  describe "fixtures integration tests" $ do
    it "infers correctly based on simple addition" $
       "example-simple-1.f90" `unitsInferReportIs` exampleInferSimple1Report
    describe "Polymorphic functions" $
      it "squarePoly1" $
        "squarePoly1.f90" `unitsInferReportIs` squarePoly1Report
    describe "Recursive functions" $
      it "Recursive Addition is OK" $
        "recursive1.f90" `unitsInferReportIs` recursive1Report
    describe "Explicitly annotated parametric polymorphic unit variables" $ do
      it "inside-outside" $
        "insideOutside.f90" `unitsInferReportIs` insideOutsideReport
      it "eapVarScope" $
        "eapVarScope.f90" `unitsInferReportIs` eapVarScopeReport
      it "eapVarApp" $
        "eapVarApp.f90" `unitsInferReportIs` eapVarAppReport
    describe "Implicit parametric polymorphic unit variables" $
      it "inferPoly1" $
        "inferPoly1.f90" `unitsInferReportIs` inferPoly1Report
    describe "Intrinsic functions" $
      it "sqrtPoly" $
        "sqrtPoly.f90" `unitsInferReportIs` sqrtPolyReport
    describe "Intrinsic function transfer (explicit cast)" $
      it "transfer" $
        "transfer.f90" `unitsInferReportIs` transferReport
    describe "GCD of powers" $
      it "gcd1" $
        "gcd1.f90" `unitsInferReportIs` gcd1Report
    describe "literals" $ do
      it "literal-zero" $
        "literal-zero.f90" `unitsInferReportIs` literalZeroReport
      it "literal-nonzero" $
        "literal-nonzero.f90" `unitsInferReportIs` literalNonZeroReport
      it "literal-nonzero" $
        "literal-nonzero2.f90" `unitsInferReportIs` literalNonZero2Report
      it "do-loop1" $
        "do-loop1.f90" `unitsInferReportIs` doLoop1Report
      it "do-loop2" $
        "do-loop2.f90" `unitsInferReportIs` doLoop2Report
    describe "cross module analysis" $ do
      it "with literals" $
        unitsInferReportWithMod ["cross-module-b/cross-module-b1.f90"] "cross-module-b/cross-module-b2.f90"
          crossModuleBReport
      it "units-suggest per file vs whole directory" singleFileVsDirectory

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units inference on a file is as expected.
unitsInferReportIs :: String -> String -> Expectation
unitsInferReportIs fileName expectedReport = do
  unitsInferReportWithMod [] fileName expectedReport

-- | Assert that the report of performing units inference on a file is as expected (with mod files).
unitsInferReportWithModAux :: LiteralsOpt -> [String] -> String -> IO String
unitsInferReportWithModAux litmod modPaths file = do      
  modFiles <- mapM mkTestModFile modPaths
  out <- readParseSrcDir Nothing modFiles file []
  putStrLn $ show out

  let [(pf,_)] = out

  let uEnv = UnitEnv { unitOpts = uOpts, unitProgramFile = pf }

  report <- runAnalysisT file (logOutputNone True) LogError modFiles $ runUnitAnalysis uEnv $ inferUnits
  let res = report ^?! arResult . _ARSuccess
  return $ show res
  where uOpts = unitOpts0 { uoLiterals = litmod }

-- | Assert that the report of performing units inference on a file is as expected (with mod files).
unitsInferReportWithMod :: [String] -> String -> String -> Expectation
unitsInferReportWithMod modNames fileName expectedReport = do
  let modPaths = fmap (fixturesDir </>) modNames
  res <- unitsInferReportWithModAux LitMixed modPaths (fixturesDir </> fileName)
  res `normalisedShouldBe` expectedReport
  where uOpts = unitOpts0 { uoLiterals = LitMixed }

-- | Helper for producing a basic ModFile from a (terminal) module file.
mkTestModFile :: String -> IO ModFile
mkTestModFile file = head <$> genModFiles Nothing emptyModFiles compileUnits unitOpts0 file []

exampleInferSimple1Report :: String
exampleInferSimple1Report =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "example-simple-1.f90:3:13 unit s :: x\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "example-simple-1.f90:3:16 unit s :: y\n"

inferReport :: String -> String -> String
inferReport fname res = concat ["\n", fixturesDir </> fname, ":\n", res]


singleFileVsDirectory :: Expectation
singleFileVsDirectory = do
  -- Top-level directory
  let fixturesDir1 = fixturesDir </> "cross-module-d"
  -- Main file
  let mainFile = fixturesDir1 </> "main.f90"
  -- and its dependencies
  let dependencies = [ fixturesDir1 </> "constants.f90", fixturesDir1 </> "functions.f90" ]
  let dependenciesMods = [ fixturesDir1 </> "constants.fsmod", fixturesDir1 </> "functions.fsmod" ]
  -- Clean up any stale .fsmod files from previous runs
  mapM_ (\f -> doesFileExist f >>= \exists -> when exists (removeFile f)) dependenciesMods
  -- Setup the environment
  let literalsOpt = LitPoly
  let camFortEnv file = CamfortEnv { ceInputSources = file
                                    , ceIncludeDir = Nothing
                                    , ceExcludeFiles = []
                                    , ceLogLevel = LogError
                                    , ceSourceSnippets = False
                                    , ceFortranVersion = Just Fortran90 }
  -- Compile the dependencies
  mapM_ (\file -> unitsCompile literalsOpt False (camFortEnv file)) dependencies
  -- Infer for the main file
  singeFileOutput <- unitsInferReportWithModAux LitPoly dependenciesMods mainFile

  -- Infer for the whole directory
  -- Remove the existing dependencies' mod files
  mapM_ removeFile dependenciesMods
  -- Compile the whole directory
  unitsCompile literalsOpt False (camFortEnv fixturesDir1)
  -- Now do inference
  multipeFileOutput <- unitsInferReportWithModAux LitPoly dependenciesMods mainFile

  -- Clean up .fsmod files to avoid polluting other tests
  mapM_ (\f -> doesFileExist f >>= \exists -> when exists (removeFile f)) dependenciesMods

  -- Compare the results
  singeFileOutput `shouldBe` multipeFileOutput

squarePoly1Report :: String
squarePoly1Report = "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "squarePoly1.f90:4:10 unit m**2 :: x\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "squarePoly1.f90:5:10 unit s**2 :: y\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "squarePoly1.f90:7:10 unit m :: a\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "squarePoly1.f90:9:10 unit s :: b\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "squarePoly1.f90:13:2 unit ('b)**2 :: square\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "squarePoly1.f90:14:12 unit 'b :: n\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "squarePoly1.f90:17:2 unit ('a)**2 :: squarep\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "squarePoly1.f90:18:12 unit 'a :: m\n"

recursive1Report :: String
recursive1Report = "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "recursive1.f90:3:13 unit 1 :: x\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "recursive1.f90:3:20 unit m :: y\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "recursive1.f90:3:27 unit m :: z\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "recursive1.f90:7:2 unit 'a :: r\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "recursive1.f90:8:15 unit 1 :: n\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "recursive1.f90:8:18 unit 'a :: b\n"

insideOutsideReport :: String
insideOutsideReport = "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "insideOutside.f90:5:12 unit 'a :: x\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "insideOutside.f90:5:15 unit 'a :: k\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "insideOutside.f90:5:18 unit ('a)**2 :: m\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "insideOutside.f90:5:21 unit ('a)**2 :: outside\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "insideOutside.f90:12:14 unit 'a :: y\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "insideOutside.f90:12:17 unit ('a)**2 :: inside\n"

eapVarScopeReport :: String
eapVarScopeReport = "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarScope.f90:5:12 unit 'a :: x\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarScope.f90:5:15 unit ('a)**3 :: k\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarScope.f90:5:18 unit ('a)**3 :: f\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarScope.f90:11:12 unit 'a :: y\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarScope.f90:11:15 unit 'a :: j\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarScope.f90:11:18 unit 'a :: g\n"

eapVarAppReport :: String
eapVarAppReport = "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:5:12 unit 'a :: fx\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:5:16 unit 'a :: fj\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:5:20 unit ('a)**2 :: fk\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:5:24 unit ('a)**4 :: fl\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:5:28 unit ('a)**2 :: f\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:13:12 unit 'b :: gx\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:13:16 unit 'b :: gn\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:13:20 unit 'b :: gm\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:13:24 unit 'b :: g\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:20:12 unit m :: hx\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:20:16 unit m**2 :: h\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "eapVarApp.f90:20:19 unit m**2 :: hy\n"

inferPoly1Report :: String
inferPoly1Report = "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "inferPoly1.f90:4:12 unit 'c :: x1\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "inferPoly1.f90:4:16 unit 'c :: id\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "inferPoly1.f90:8:12 unit 'f :: x2\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "inferPoly1.f90:8:16 unit ('f)**2 :: sqr\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "inferPoly1.f90:12:12 unit 'a :: x3\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "inferPoly1.f90:12:16 unit 'b :: y3\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "inferPoly1.f90:12:20 unit 'a :: fst\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "inferPoly1.f90:16:12 unit 'e :: x4\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "inferPoly1.f90:16:16 unit 'd :: y4\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "inferPoly1.f90:16:20 unit 'd :: snd\n"

sqrtPolyReport :: String
sqrtPolyReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "sqrtPoly.f90:4:10 unit m :: x\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "sqrtPoly.f90:6:10 unit s :: y\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "sqrtPoly.f90:8:10 unit j :: z\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "sqrtPoly.f90:9:13 unit m**2 :: a\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "sqrtPoly.f90:10:13 unit s**4 :: b\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "sqrtPoly.f90:11:13 unit j**2 :: c\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "sqrtPoly.f90:16:2 unit ('a)**2 :: square\n\
  \tests" </> "fixtures" </> "Specification" </> "Units" </> "sqrtPoly.f90:17:12 unit 'a :: n\n"

transferReport :: String
transferReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "transfer.f90:4:10 unit m :: x\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "transfer.f90:6:10 unit s :: y\n"

gcd1Report :: String
gcd1Report =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "gcd1.f90:3:2 unit ('a)**12 :: g\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "gcd1.f90:4:12 unit ('a)**2 :: x\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "gcd1.f90:4:15 unit ('a)**3 :: y\n"

literalZeroReport :: String
literalZeroReport = "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "literal-zero.f90:3:10 unit m :: a\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-zero.f90:3:13 unit m :: b\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-zero.f90:9:2 unit 'a :: f\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-zero.f90:11:12 unit 'a :: x\n"

literalNonZeroReport :: String
literalNonZeroReport = "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero.f90:2:10 unit m s :: a\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero.f90:2:13 unit m s :: b\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero.f90:8:2 unit m s :: f\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero.f90:10:12 unit m s :: x\n"

literalNonZero2Report :: String
literalNonZero2Report = "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero2.f90:3:10 unit m :: a\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero2.f90:3:13 unit m :: b\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero2.f90:3:16 unit m :: c\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero2.f90:3:19 unit m :: d\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero2.f90:4:21 unit m :: n\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero2.f90:10:2 unit m :: f\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "literal-nonzero2.f90:11:12 unit m :: x\n"

doLoop1Report :: String
doLoop1Report = "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop1.f90:3:10 unit m :: x\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop1.f90:3:13 unit m :: y\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop1.f90:4:13 unit m :: i\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop1.f90:10:2 unit 1 :: f\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop1.f90:11:12 unit 1 :: x\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop1.f90:11:15 unit 1 :: y\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop1.f90:12:15 unit 1 :: i\n"

doLoop2Report :: String
doLoop2Report = "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:3:10 unit m :: x\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:3:13 unit m :: y\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:4:13 unit m :: i\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:10:2 unit 1 :: f\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:11:12 unit 1 :: x\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:11:15 unit 1 :: y\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:12:15 unit 1 :: i\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:19:2 unit 1 :: g\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:20:12 unit 1 :: x\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:20:15 unit 1 :: y\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:21:15 unit 1 :: i\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:28:2 unit 'a :: h\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:29:12 unit 'a :: x\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:29:15 unit 'a :: y\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "do-loop2.f90:30:15 unit 'a :: i\n"

crossModuleBReport :: String
crossModuleBReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-b" </> "cross-module-b2.f90:6:23 unit c :: foo\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-b" </> "cross-module-b2.f90:9:12 unit c :: tc\n\
\tests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-b" </> "cross-module-b2.f90:9:16 unit k :: t\n"
