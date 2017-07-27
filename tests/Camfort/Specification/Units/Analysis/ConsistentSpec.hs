module Camfort.Specification.Units.Analysis.ConsistentSpec (spec) where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Camfort.Analysis.Fortran (analysisResult)
import Camfort.Analysis.ModFile (getModFiles)
import Camfort.Input (readParseSrcDirWithModFiles)
import Camfort.Specification.Units.Analysis (runUnitsAnalysis)
import Camfort.Specification.Units.Analysis.Consistent (checkUnits)
import Camfort.Specification.Units.Monad
  (LiteralsOpt(..), unitOpts0, uoDebug, uoLiterals)

spec :: Test.Spec
spec =
  describe "consistency analysis" $ do
    it "reports (simple) inconsistent units" $
       "example-inconsist-1.f90" `unitsCheckReportIs` exampleInconsist1CheckReport
    it "Polymorphic non-zero literal is not OK" $
       "inconsistLitInPolyFun.f90" `unitsCheckReportIs` inconsistLitInPolyFunReport
    it "Recursive Multiplication is not OK" $
       "inconsistRecMult.f90" `unitsCheckReportIs` inconsistRecMultReport
    describe "reports with varying Literal Modes" $ do
      it "LitMixed" $
        unitsCheckReport LitMixed    "inconsist3.f90" inconsist3LitMixedReport
      it "LitPoly" $
        unitsCheckReport LitPoly     "inconsist3.f90" inconsist3LitPolyReport
      it "LitUnitless" $
        unitsCheckReport LitUnitless "inconsist3.f90" inconsist3LitUnitlessReport

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units checking on a file is as expected.
unitsCheckReport :: LiteralsOpt -> String -> String -> Expectation
unitsCheckReport lo fileName expectedReport = do
  let file = fixturesDir </> fileName
  incDir <- getCurrentDirectory
  [(pf,_)] <- readParseSrcDirWithModFiles file incDir []
  modFiles <- getModFiles incDir
  let report = analysisResult $ runUnitsAnalysis checkUnits uOpts modFiles pf
  show report `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoDebug = False, uoLiterals = lo }

-- | Assert that the report of performing units checking on a file is as expected.
unitsCheckReportIs :: String -> String -> Expectation
unitsCheckReportIs = unitsCheckReport LitMixed

exampleInconsist1CheckReport :: String
exampleInconsist1CheckReport =
  "\ntests/fixtures/Specification/Units/example-inconsist-1.f90: Inconsistent:\n\
  \ - at 7:7: 'z' should have unit 's'\n\
  \ - at 7:7: Units 's' and 'm' should be equal\n\n\n\
  \(7:7)-(7:11): s === m\n\
  \(7:3)-(7:11): unit_of(z) === s\n\
  \(1:1)-(8:19): unit_of(z) === s && s === m\n"

inconsist3LitMixedReport :: String
inconsist3LitMixedReport =
  "\ntests/fixtures/Specification/Units/inconsist3.f90: Inconsistent:\n\
  \ - at 5:7: 'j' should have unit 'literal'\n\
  \ - at 5:7: 'literal' should have unit 'literal'\n\
  \ - at 7:7: 'k' should have the same units as '(unit_of(j)) (unit_of(j))'\n\n\n\
  \(5:7)-(5:11): #<Literal id=0> === #<Literal id=1>\n\
  \(7:7)-(7:11): a === unit_of(k)\n\
  \(5:3)-(5:11): unit_of(j) === #<Literal id=0>\n\
  \(6:3)-(6:11): unit_of(k) === (unit_of(j)) (unit_of(j))\n\
  \(7:3)-(7:11): a === a\n\
  \(8:3)-(8:11): a === a (unit_of(j))\n\
  \(1:1)-(9:19): unit_of(j) === #<Literal id=0> && #<Literal id=0> === #<Literal id=1> && unit_of(k) === (unit_of(j)) (unit_of(j)) && a === a && a === unit_of(k) && a === a (unit_of(j))\n"

inconsist3LitPolyReport :: String
inconsist3LitPolyReport =
  "\ntests/fixtures/Specification/Units/inconsist3.f90: Inconsistent:\n\
  \ - at 7:7: 'k' should have the same units as '(unit_of(j)) (unit_of(j))'\n\n\n\
  \(5:7)-(5:11): #<ParamLitAbs litId=0> === #<ParamLitAbs litId=1>\n\
  \(7:7)-(7:11): a === unit_of(k)\n\
  \(5:3)-(5:11): unit_of(j) === #<ParamLitAbs litId=0>\n\
  \(6:3)-(6:11): unit_of(k) === (unit_of(j)) (unit_of(j))\n\
  \(7:3)-(7:11): a === a\n\
  \(8:3)-(8:11): a === a (unit_of(j))\n\
  \(1:1)-(9:19): unit_of(j) === #<ParamLitAbs litId=0> && #<ParamLitAbs litId=0> === #<ParamLitAbs litId=1> && unit_of(k) === (unit_of(j)) (unit_of(j)) && a === a && a === unit_of(k) && a === a (unit_of(j))\n"

inconsist3LitUnitlessReport :: String
inconsist3LitUnitlessReport =
  "\ntests/fixtures/Specification/Units/inconsist3.f90: Inconsistent:\n\
  \ - at 5:7: 'j' should have unit '1'\n\n\n\
  \(5:7)-(5:11): 1 === 1\n\
  \(7:7)-(7:11): a === unit_of(k)\n\
  \(5:3)-(5:11): unit_of(j) === 1\n\
  \(6:3)-(6:11): unit_of(k) === (unit_of(j)) (unit_of(j))\n\
  \(7:3)-(7:11): a === a\n\
  \(8:3)-(8:11): a === a (unit_of(j))\n\
  \(1:1)-(9:19): unit_of(j) === 1 && 1 === 1 && unit_of(k) === (unit_of(j)) (unit_of(j)) && a === a && a === unit_of(k) && a === a (unit_of(j))\n"

inconsistLitInPolyFunReport :: String
inconsistLitInPolyFunReport =
  "\ntests/fixtures/Specification/Units/inconsistLitInPolyFun.f90: Inconsistent:\n\
  \ - at 10:3: 'a' should have the same units as 'result of sqr'\n\
  \ - at 10:11: 'literal' should have unit 'm'\n\
  \ - at 10:11: 'parameter 1 to sqr' should have unit 'm'\n\
  \ - at 11:11: 'literal' should have unit 's'\n\n\n\
  \(16:11)-(16:19): (#<ParamPosAbs example_sqr5[1]>) (#<ParamPosAbs example_sqr5[1]>) === #<ParamVarAbs example_sqr5.example_sqr_z7>\n\
  \(10:3)-(10:12): unit_of(a) === #<ParamPosUse example_sqr5[0] callId=0>\n\
  \(11:3)-(11:12): unit_of(b) === #<ParamPosUse example_sqr5[0] callId=1>\n\
  \(16:5)-(16:19): #<ParamPosAbs example_sqr5[0]> === (#<ParamPosAbs example_sqr5[1]>) (#<ParamPosAbs example_sqr5[1]>)\n\
  \(10:11)-(10:11): m === #<ParamPosUse example_sqr5[1] callId=0>\n\
  \(11:11)-(11:11): s === #<ParamPosUse example_sqr5[1] callId=1>\n\
  \(7:11)-(7:15): m === #<Literal id=1>\n\
  \(9:11)-(9:15): s === #<Literal id=3>\n\
  \(15:13)-(15:18): #<ParamVarAbs example_sqr5.example_sqr_z7> === #<Literal id=4>\n\
  \(13:3)-(17:14): #<ParamVarAbs example_sqr5.example_sqr5> === #<ParamPosAbs example_sqr5[0]> && #<ParamVarAbs example_sqr5.example_sqr_y6> === #<ParamPosAbs example_sqr5[1]> && #<ParamVarAbs example_sqr5.example_sqr_z7> === #<Literal id=4> && #<ParamPosAbs example_sqr5[0]> === (#<ParamPosAbs example_sqr5[1]>) (#<ParamPosAbs example_sqr5[1]>) && (#<ParamPosAbs example_sqr5[1]>) (#<ParamPosAbs example_sqr5[1]>) === #<ParamVarAbs example_sqr5.example_sqr_z7>\n\
  \(3:1)-(18:19): m === #<Literal id=0> && m === #<Literal id=1> && s === #<Literal id=2> && s === #<Literal id=3> && unit_of(a) === #<ParamPosUse example_sqr5[0] callId=0> && m === #<ParamPosUse example_sqr5[1] callId=0> && unit_of(b) === #<ParamPosUse example_sqr5[0] callId=1> && s === #<ParamPosUse example_sqr5[1] callId=1> && #<ParamVarAbs example_sqr5.example_sqr5> === #<ParamPosAbs example_sqr5[0]> && #<ParamVarAbs example_sqr5.example_sqr_y6> === #<ParamPosAbs example_sqr5[1]> && #<ParamVarAbs example_sqr5.example_sqr_z7> === #<Literal id=4> && #<ParamPosAbs example_sqr5[0]> === (#<ParamPosAbs example_sqr5[1]>) (#<ParamPosAbs example_sqr5[1]>) && (#<ParamPosAbs example_sqr5[1]>) (#<ParamPosAbs example_sqr5[1]>) === #<ParamVarAbs example_sqr5.example_sqr_z7> && #<ParamVarAbs example_sqr5.example_sqr_z7> === #<Literal id=4> && #<ParamPosAbs example_sqr5[0]> === (#<ParamPosAbs example_sqr5[1]>) (#<ParamPosAbs example_sqr5[1]>) && (#<ParamPosAbs example_sqr5[1]>) (#<ParamPosAbs example_sqr5[1]>) === #<ParamVarAbs example_sqr5.example_sqr_z7>\n"

inconsistRecMultReport :: String
inconsistRecMultReport =
  "\ntests/fixtures/Specification/Units/inconsistRecMult.f90: Inconsistent:\n\
  \ - at 4:3: 'z' should have the same units as 'result of recur'\n\
  \ - at 4:13: 'x' should have unit 'literal'\n\
  \ - at 4:15: 'literal' should have unit 'm'\n\
  \ - at 9:9: 'x' should have the same units as 'parameter 1 to recur'\n\
  \ - at 10:8: 'parameter 2 to recur' should have unit 'm'\n\n\n\
  \(9:9)-(9:16): #<ParamPosAbs main_recur4[1]> === #<ParamLitAbs litId=0>\n\
  \(12:22)-(12:26): #<ParamPosAbs main_recur4[1]> === 1\n\
  \(4:3)-(4:16): unit_of(z) === #<ParamPosUse main_recur4[0] callId=0>\n\
  \(10:8)-(10:12): #<ParamPosAbs main_recur4[0]> === #<ParamPosAbs main_recur4[2]>\n\
  \(12:8)-(12:30): #<ParamPosAbs main_recur4[0]> === (#<ParamPosAbs main_recur4[2]>) (#<ParamPosUse main_recur4[0] callId=1>)\n\
  \(4:13)-(4:13): unit_of(x) === #<ParamPosUse main_recur4[1] callId=0>\n\
  \(4:15)-(4:15): m === #<ParamPosUse main_recur4[2] callId=0>\n\
  \(12:22)-(12:26): #<ParamPosAbs main_recur4[1]> === #<ParamPosUse main_recur4[1] callId=1>\n\
  \(12:29)-(12:29): #<ParamPosAbs main_recur4[2]> === #<ParamPosUse main_recur4[2] callId=1>\n\
  \(3:14)-(3:18): unit_of(x) === #<Literal id=3>\n\
  \(3:21)-(3:25): m === #<Literal id=4>\n\
  \(7:3)-(14:20): #<ParamVarAbs main_recur4.main_recur_r5> === #<ParamPosAbs main_recur4[0]> && #<ParamVarAbs main_recur4.main_recur_n6> === #<ParamPosAbs main_recur4[1]> && #<ParamVarAbs main_recur4.main_recur_b7> === #<ParamPosAbs main_recur4[2]> && #<ParamPosAbs main_recur4[1]> === #<ParamLitAbs litId=0> && #<ParamPosAbs main_recur4[0]> === #<ParamPosAbs main_recur4[2]> && #<ParamPosAbs main_recur4[0]> === (#<ParamPosAbs main_recur4[2]>) (#<ParamPosUse main_recur4[0] callId=1>) && #<ParamPosAbs main_recur4[1]> === #<ParamPosUse main_recur4[1] callId=1> && #<ParamPosAbs main_recur4[1]> === 1 && #<ParamPosAbs main_recur4[2]> === #<ParamPosUse main_recur4[2] callId=1>\n\
  \(1:1)-(15:16): unit_of(x) === #<Literal id=1> && m === #<Literal id=2> && unit_of(x) === #<Literal id=3> && m === #<Literal id=4> && unit_of(z) === #<ParamPosUse main_recur4[0] callId=0> && unit_of(x) === #<ParamPosUse main_recur4[1] callId=0> && m === #<ParamPosUse main_recur4[2] callId=0> && #<ParamVarAbs main_recur4.main_recur_r5> === #<ParamPosAbs main_recur4[0]> && #<ParamVarAbs main_recur4.main_recur_n6> === #<ParamPosAbs main_recur4[1]> && #<ParamVarAbs main_recur4.main_recur_b7> === #<ParamPosAbs main_recur4[2]> && #<ParamPosAbs main_recur4[1]> === #<ParamLitAbs litId=0> && #<ParamPosAbs main_recur4[0]> === #<ParamPosAbs main_recur4[2]> && #<ParamPosAbs main_recur4[0]> === (#<ParamPosAbs main_recur4[2]>) (#<ParamPosUse main_recur4[0] callId=1>) && #<ParamPosAbs main_recur4[1]> === #<ParamPosUse main_recur4[1] callId=1> && #<ParamPosAbs main_recur4[1]> === 1 && #<ParamPosAbs main_recur4[2]> === #<ParamPosUse main_recur4[2] callId=1> && #<ParamPosAbs main_recur4[1]> === #<ParamLitAbs litId=0> && #<ParamPosAbs main_recur4[0]> === #<ParamPosAbs main_recur4[2]> && #<ParamPosAbs main_recur4[0]> === (#<ParamPosAbs main_recur4[2]>) (#<ParamPosUse main_recur4[0] callId=1>) && #<ParamPosAbs main_recur4[1]> === #<ParamPosUse main_recur4[1] callId=1> && #<ParamPosAbs main_recur4[1]> === 1 && #<ParamPosAbs main_recur4[2]> === #<ParamPosUse main_recur4[2] callId=1>\n"
