module Camfort.Specification.Units.Analysis.InferSpec (spec) where

import System.FilePath ((</>))

import Control.Lens

import           Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Language.Fortran.Util.ModFile (ModFile, emptyModFiles)

import Camfort.Analysis hiding (describe)
import Camfort.Analysis.ModFile (genModFiles, readParseSrcDir)
import Camfort.Specification.Units.Analysis (compileUnits)
import Camfort.Specification.Units.Analysis.Infer (inferUnits)
import Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoLiterals, runUnitAnalysis, UnitEnv(..))

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



fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units inference on a file is as expected.
unitsInferReportIs :: String -> String -> Expectation
unitsInferReportIs fileName expectedReport = do
  unitsInferReportWithMod [] fileName expectedReport

-- | Assert that the report of performing units inference on a file is as expected (with mod files).
unitsInferReportWithMod :: [String] -> String -> String -> Expectation
unitsInferReportWithMod modNames fileName expectedReport = do
  let file = fixturesDir </> fileName
      modPaths = fmap (fixturesDir </>) modNames
  modFiles <- mapM mkTestModFile modPaths
  [(pf,_)] <- readParseSrcDir Nothing modFiles file []

  let uEnv = UnitEnv { unitOpts = uOpts, unitProgramFile = pf }

  report <- runAnalysisT file (logOutputNone True) LogError modFiles $ runUnitAnalysis uEnv $ inferUnits
  let res = report ^?! arResult . _ARSuccess

  show res `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoLiterals = LitMixed }

-- | Helper for producing a basic ModFile from a (terminal) module file.
mkTestModFile :: String -> IO ModFile
mkTestModFile file = head <$> genModFiles Nothing emptyModFiles compileUnits unitOpts0 file []

exampleInferSimple1Report :: String
exampleInferSimple1Report =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "example-simple-1.f90:\n\
  \  3:14 unit s :: x\n\
  \  3:17 unit s :: y\n"

inferReport :: String -> String -> String
inferReport fname res = concat ["\n", fixturesDir </> fname, ":\n", res]

squarePoly1Report :: String
squarePoly1Report = inferReport "squarePoly1.f90"
  "  4:11 unit m**2 :: x\n\
  \  5:11 unit s**2 :: y\n\
  \  7:11 unit m :: a\n\
  \  9:11 unit s :: b\n\
  \  13:3 unit ('b)**2 :: square\n\
  \  14:13 unit 'b :: n\n\
  \  17:3 unit ('a)**2 :: squarep\n\
  \  18:13 unit 'a :: m\n"

recursive1Report :: String
recursive1Report = inferReport "recursive1.f90"
  "  3:14 unit 1 :: x\n\
  \  3:21 unit m :: y\n\
  \  3:28 unit m :: z\n\
  \  7:3 unit 'a :: r\n\
  \  8:16 unit 1 :: n\n\
  \  8:19 unit 'a :: b\n"

insideOutsideReport :: String
insideOutsideReport = inferReport "insideOutside.f90"
  "  5:13 unit 'a :: x\n\
  \  5:16 unit 'a :: k\n\
  \  5:19 unit ('a)**2 :: m\n\
  \  5:22 unit ('a)**2 :: outside\n\
  \  12:15 unit 'a :: y\n\
  \  12:18 unit ('a)**2 :: inside\n"

eapVarScopeReport :: String
eapVarScopeReport = inferReport "eapVarScope.f90"
  "  5:13 unit 'a :: x\n\
  \  5:16 unit ('a)**3 :: k\n\
  \  5:19 unit ('a)**3 :: f\n\
  \  11:13 unit 'a :: y\n\
  \  11:16 unit 'a :: j\n\
  \  11:19 unit 'a :: g\n"

eapVarAppReport :: String
eapVarAppReport = inferReport "eapVarApp.f90"
  "  5:13 unit 'a :: fx\n\
  \  5:17 unit 'a :: fj\n\
  \  5:21 unit ('a)**2 :: fk\n\
  \  5:25 unit ('a)**4 :: fl\n\
  \  5:29 unit ('a)**2 :: f\n\
  \  13:13 unit 'b :: gx\n\
  \  13:17 unit 'b :: gn\n\
  \  13:21 unit 'b :: gm\n\
  \  13:25 unit 'b :: g\n\
  \  20:13 unit m :: hx\n\
  \  20:17 unit m**2 :: h\n\
  \  20:20 unit m**2 :: hy\n"

inferPoly1Report :: String
inferPoly1Report = inferReport "inferPoly1.f90"
  "  4:13 unit 'c :: x1\n\
  \  4:17 unit 'c :: id\n\
  \  8:13 unit 'f :: x2\n\
  \  8:17 unit ('f)**2 :: sqr\n\
  \  12:13 unit 'a :: x3\n\
  \  12:17 unit 'b :: y3\n\
  \  12:21 unit 'a :: fst\n\
  \  16:13 unit 'e :: x4\n\
  \  16:17 unit 'd :: y4\n\
  \  16:21 unit 'd :: snd\n"

sqrtPolyReport :: String
sqrtPolyReport = inferReport "sqrtPoly.f90"
  "  4:11 unit m :: x\n\
  \  6:11 unit s :: y\n\
  \  8:11 unit j :: z\n\
  \  9:14 unit m**2 :: a\n\
  \  10:14 unit s**4 :: b\n\
  \  11:14 unit j**2 :: c\n\
  \  16:3 unit ('a)**2 :: square\n\
  \  17:13 unit 'a :: n\n"

transferReport :: String
transferReport = inferReport "transfer.f90"
  "  4:11 unit m :: x\n\
  \  6:11 unit s :: y\n"

gcd1Report :: String
gcd1Report = inferReport "gcd1.f90"
  "  3:3 unit ('a)**12 :: g\n\
  \  4:13 unit ('a)**2 :: x\n\
  \  4:16 unit ('a)**3 :: y\n"

literalZeroReport :: String
literalZeroReport = inferReport "literal-zero.f90"
  "  3:11 unit m :: a\n\
  \  3:14 unit m :: b\n\
  \  9:3 unit 'a :: f\n\
  \  11:13 unit 'a :: x\n"

literalNonZeroReport :: String
literalNonZeroReport = inferReport "literal-nonzero.f90"
  "  2:11 unit m s :: a\n\
  \  2:14 unit m s :: b\n\
  \  8:3 unit m s :: f\n\
  \  10:13 unit m s :: x\n"

literalNonZero2Report :: String
literalNonZero2Report = inferReport "literal-nonzero2.f90"
  "  3:11 unit m :: a\n\
  \  3:14 unit m :: b\n\
  \  3:17 unit m :: c\n\
  \  3:20 unit m :: d\n\
  \  4:22 unit m :: n\n\
  \  10:3 unit m :: f\n\
  \  11:13 unit m :: x\n"

doLoop1Report :: String
doLoop1Report = inferReport "do-loop1.f90"
  "  3:11 unit m :: x\n\
  \  3:14 unit m :: y\n\
  \  4:14 unit m :: i\n\
  \  10:3 unit 1 :: f\n\
  \  11:13 unit 1 :: x\n\
  \  11:16 unit 1 :: y\n\
  \  12:16 unit 1 :: i\n"

doLoop2Report :: String
doLoop2Report = inferReport "do-loop2.f90"
  "  3:11 unit m :: x\n\
  \  3:14 unit m :: y\n\
  \  4:14 unit m :: i\n\
  \  10:3 unit 1 :: f\n\
  \  11:13 unit 1 :: x\n\
  \  11:16 unit 1 :: y\n\
  \  12:16 unit 1 :: i\n\
  \  19:3 unit 1 :: g\n\
  \  20:13 unit 1 :: x\n\
  \  20:16 unit 1 :: y\n\
  \  21:16 unit 1 :: i\n\
  \  28:3 unit 'a :: h\n\
  \  29:13 unit 'a :: x\n\
  \  29:16 unit 'a :: y\n\
  \  30:16 unit 'a :: i\n"

crossModuleBReport :: String
crossModuleBReport =
  "\ntests" </> "fixtures" </> "Specification" </> "Units" </> "cross-module-b" </> "cross-module-b2.f90:\n\
  \  6:24 unit c :: foo\n\
  \  9:13 unit c :: tc\n\
  \  9:17 unit k :: t\n"
