module Camfort.Specification.Units.Analysis.InferSpec (spec) where

import System.FilePath ((</>))

import           Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Language.Fortran.Util.ModFile (emptyModFiles)

import Camfort.Analysis (analysisResult)
import Camfort.Analysis.ModFile (readParseSrcDir)
import Camfort.Specification.Units.Analysis (runUnitsAnalysis)
import Camfort.Specification.Units.Analysis.Infer (inferUnits)
import Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoDebug, uoLiterals)

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

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units inference on a file is as expected.
unitsInferReportIs :: String -> String -> Expectation
unitsInferReportIs fileName expectedReport = do
  let file = fixturesDir </> fileName
  let modFiles = emptyModFiles
  [(pf,_)] <- readParseSrcDir modFiles file []
  let (Right report) = analysisResult $ runUnitsAnalysis inferUnits uOpts modFiles pf
  show report `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoDebug = False, uoLiterals = LitMixed }

exampleInferSimple1Report :: String
exampleInferSimple1Report =
  "\ntests/fixtures/Specification/Units/example-simple-1.f90:\n\
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
  \  13:3 unit ('a)**2 :: square\n\
  \  14:13 unit 'a :: n\n\
  \  17:3 unit ('b)**2 :: squarep\n\
  \  18:13 unit 'b :: m\n"

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
