module Camfort.Specification.Units.Analysis.InferSpec (spec) where

import qualified Data.ByteString.Char8 as B
import           Data.Generics.Uniplate.Operations (universeBi)
import           Data.List (nub, sort)
import           Data.Maybe (mapMaybe, maybeToList)
import           System.FilePath ((</>))

import           Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import           Language.Fortran.Parser.Any (fortranParser)
import           Language.Fortran.ParserMonad (fromRight)
import           Language.Fortran.Util.ModFile (emptyModFiles)

import           Camfort.Analysis (analysisResult, finalState)
import           Camfort.Analysis.ModFile (readParseSrcDir)
import           Camfort.Analysis.Annotations (unitAnnotation)
import           Camfort.Specification.Units.Analysis
  (initInference, runUnitsAnalysis)
import           Camfort.Specification.Units.Analysis.Infer
  (inferUnits, runInferVariables)
import           Camfort.Specification.Units.Annotation (UA)
import qualified Camfort.Specification.Units.Annotation as UA
import           Camfort.Specification.Units.Environment
  (Constraints, UnitInfo)
import           Camfort.Specification.Units.InferenceBackend (chooseImplicitNames)
import           Camfort.Specification.Units.Monad
  ( LiteralsOpt(..)
  , UnitSolver, runUnitSolver
  , unitOpts0, uoDebug, uoLiterals
  , usConstraints )

spec :: Test.Spec
spec = do
  let showClean = show . nub . sort . fst
  describe "runInferVariables" $ do
    describe "Polymorphic functions" $
      it "squarePoly1" $
        showClean (runUnits LitMixed squarePoly1 (fmap chooseImplicitNames runInferVariables)) `shouldBe`
          "[((\"a\",\"a\"),m),((\"b\",\"b\"),s),((\"m\",\"m\"),'b),((\"n\",\"n\"),'a),((\"square\",\"square\"),('a)**2),((\"squarep\",\"squarep\"),('b)**2),((\"x\",\"x\"),m**2),((\"y\",\"y\"),s**2)]"
    describe "Recursive functions" $
      it "Recursive Addition is OK" $
        showClean (runUnits LitMixed recursive1 (fmap chooseImplicitNames runInferVariables)) `shouldBe`
          "[((\"b\",\"b\"),'a),((\"n\",\"n\"),1),((\"r\",\"r\"),'a),((\"x\",\"x\"),1),((\"y\",\"y\"),m),((\"z\",\"z\"),m)]"
    describe "Explicitly annotated parametric polymorphic unit variables" $ do
      it "inside-outside" $
        showClean (runUnits LitMixed insideOutside runInferVariables) `shouldBe`
          "[((\"inside\",\"inside\"),('a)**2),((\"k\",\"k\"),'a),((\"m\",\"m\"),('a)**2),((\"outside\",\"outside\"),('a)**2),((\"x\",\"x\"),'a),((\"y\",\"y\"),'a)]"
      it "eapVarScope" $
        show (sort (fst (runUnitInference LitMixed eapVarScope))) `shouldBe`
          "[(\"f\",('a)**3),(\"g\",'a),(\"j\",'a),(\"k\",('a)**3),(\"x\",'a),(\"y\",'a)]"
      it "eapVarApp" $
        show (sort (fst (runUnitInference LitMixed eapVarApp))) `shouldBe`
          "[(\"f\",('a)**2),(\"fj\",'a),(\"fk\",('a)**2),(\"fl\",('a)**4),(\"fx\",'a),(\"g\",'b),(\"gm\",'b),(\"gn\",'b),(\"gx\",'b),(\"h\",m**2),(\"hx\",m),(\"hy\",m**2)]"
    describe "Implicit parametric polymorphic unit variables" $
      it "inferPoly1" $
        show (sort (fst (runUnitInference LitMixed inferPoly1))) `shouldBe`
          "[(\"fst\",'a),(\"id\",'c),(\"snd\",'d),(\"sqr\",('f)**2),(\"x1\",'c),(\"x2\",'f),(\"x3\",'a),(\"x4\",'e),(\"y3\",'b),(\"y4\",'d)]"
    describe "Intrinsic functions" $
      it "sqrtPoly" $
        show (sort (fst (runUnitInference LitMixed sqrtPoly))) `shouldBe`
          "[(\"a\",m**2),(\"b\",s**4),(\"c\",j**2),(\"n\",'a),(\"x\",m),(\"y\",s),(\"z\",j)]"
  describe "fixtures integration tests" $
    describe "units-infer" $
      it "infers correctly based on simple addition" $
         "example-simple-1.f90" `unitsInferReportIs` exampleInferSimple1Report

runUnits :: LiteralsOpt
            -> F.ProgramFile b
            -> UnitSolver t
            -> (t, Constraints)
runUnits litMode pf m = (r, usConstraints state)
  where
    pf' = FA.initAnalysis . fmap (UA.mkUnitAnnotation . const unitAnnotation) $ pf
    uOpts = unitOpts0 { uoDebug = False, uoLiterals = litMode }
    (r, state) =
      let res = runUnitSolver uOpts pf' emptyModFiles $ initInference >> m
      in (analysisResult res, finalState res)

runUnitInference :: LiteralsOpt
                 -> F.ProgramFile b
                 -> ([(String, UnitInfo)], Constraints)
runUnitInference litMode pf =
  ([ (FA.varName e, u) | e <- declVariables pf'
                       , u <- maybeToList ((FA.varName e, FA.srcName e) `lookup` vars) ]
  , usConstraints state)
  where
    pf' = FA.initAnalysis . fmap (UA.mkUnitAnnotation . const unitAnnotation) $ pf
    uOpts = unitOpts0 { uoDebug = False, uoLiterals = litMode }
    (vars, state) =
      let res = runUnitSolver uOpts pf' emptyModFiles $ initInference >> fmap chooseImplicitNames runInferVariables
      in (analysisResult res, finalState res)

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

declVariables :: F.ProgramFile UA -> [F.Expression UA]
declVariables pf = flip mapMaybe (universeBi pf) $ \ d -> case d of
  F.DeclVariable _ _ v@(F.ExpValue _ _ (F.ValVariable _)) _ _   -> Just v
  F.DeclArray    _ _ v@(F.ExpValue _ _ (F.ValVariable _)) _ _ _ -> Just v
  _                                                             -> Nothing

fortranParser' :: B.ByteString -> String -> F.ProgramFile F.A0
fortranParser' x = fromRight . fortranParser x

squarePoly1 :: F.ProgramFile F.A0
squarePoly1 = flip fortranParser' "squarePoly1.f90" . B.pack $ unlines
    [ "! Demonstrates parametric polymorphism through functions-calling-functions."
    , "program squarePoly"
    , "  implicit none"
    , "  real :: x"
    , "  real :: y"
    , "  != unit(m) :: a"
    , "  real :: a"
    , "  != unit(s) :: b"
    , "  real :: b"
    , "  x = squareP(a)"
    , "  y = squareP(b)"
    , "  contains"
    , "  real function square(n)"
    , "    real :: n"
    , "    square = n * n"
    , "  end function"
    , "  real function squareP(m)"
    , "    real :: m"
    , "    squareP = square(m)"
    , "  end function"
    , "end program" ]

recursive1 :: F.ProgramFile F.A0
recursive1 = flip fortranParser' "recursive1.f90" . B.pack $ unlines
    [ "program main"
    , "  != unit(m) :: y"
    , "  integer :: x = 5, y = 2, z"
    , "  z = recur(x,y)"
    , "  print *, y"
    , "contains"
    , "  real recursive function recur(n, b) result(r)"
    , "    integer :: n, b"
    , "    if (n .EQ. 0) then"
    , "       r = b"
    , "    else"
    , "       r = b + recur(n - 1, b)"
    , "    end if"
    , "  end function recur"
    , "end program main" ]

insideOutside :: F.ProgramFile F.A0
insideOutside = flip fortranParser' "insideOutside.f90" . B.pack $ unlines
    [ "module insideOutside"
    , "contains"
    , "  function outside(x)"
    , "    != unit 'a :: x"
    , "    real :: x, k, m, outside"
    , "    k = x"
    , "    outside = inside(k) * 2"
    , "    m = outside"
    , "  contains"
    , "    function inside(y)"
    , "      != unit 'a ** 2 :: inside"
    , "      real :: y, inside"
    , "      inside = y * y"
    , "    end function inside"
    , "  end function outside"
    , "end module insideOutside" ]

eapVarScope :: F.ProgramFile F.A0
eapVarScope = flip fortranParser' "eapVarScope.f90" . B.pack $ unlines
    [ "module eapVarScope"
    , "contains"
    , "  function f(x)"
    , "    != unit 'a :: x"
    , "    real :: x, k, f"
    , "    k = g(x) * g(x * x)"
    , "    f = k"
    , "  end function f"
    , "  function g(y)"
    , "    != unit 'a :: y"
    , "    real :: y, j, g"
    , "    j = y"
    , "    g = j"
    , "  end function g"
    , "end module eapVarScope" ]

eapVarApp :: F.ProgramFile F.A0
eapVarApp = flip fortranParser' "eapVarApp.f90" . B.pack $ unlines
    [ "module eapVarApp"
    , "contains"
    , "  function f(fx)"
    , "    != unit 'a :: fx"
    , "    real :: fx, fj, fk, fl, f"
    , "    fj = fx"
    , "    fk = g(fj*fj)"
    , "    fl = fj * g(fj * fj * fj)"
    , "    f = fk"
    , "  end function f"
    , "  function g(gx)"
    , "    != unit 'b :: gx"
    , "    real :: gx, gn, gm, g"
    , "    gm = gx"
    , "    gn = gm"
    , "    g = gn"
    , "  end function g"
    , "  function h(hx)"
    , "    != unit m :: hx"
    , "    real :: hx, h, hy"
    , "    hy = f(hx)"
    , "    h = hy"
    , "  end function h"
    , "end module eapVarApp" ]

inferPoly1 :: F.ProgramFile F.A0
inferPoly1 = flip fortranParser' "inferPoly1.f90" . B.pack $ unlines
    [ "module inferPoly1"
    , "contains"
    , "  function id(x1)"
    , "    real :: x1, id"
    , "    id = x1"
    , "  end function id"
    , "  function sqr(x2)"
    , "    real :: x2, sqr"
    , "    sqr = x2 * x2"
    , "  end function sqr"
    , "  function fst(x3,y3)"
    , "    real :: x3, y3, fst"
    , "    fst = x3"
    , "  end function fst"
    , "  function snd(x4,y4)"
    , "    real :: x4, y4, snd"
    , "    snd = y4"
    , "  end function snd"
    , "end module inferPoly1" ]

-- Test intrinsic function sqrt()
sqrtPoly :: F.ProgramFile F.A0
sqrtPoly = flip fortranParser' "sqrtPoly.f90" . B.pack $ unlines
    [ "program sqrtPoly"
    , "  implicit none"
    , "  != unit m :: x"
    , "  real :: x"
    , "  != unit s :: y"
    , "  real :: y"
    , "  != unit J :: z"
    , "  real :: z"
    , "  integer :: a"
    , "  integer :: b"
    , "  integer :: c"
    , "  x = sqrt(a)"
    , "  y = sqrt(sqrt(b))"
    , "  z = sqrt(square(sqrt(c)))"
    , "contains"
    , "  real function square(n)"
    , "    real :: n"
    , "    square = n * n"
    , "  end function square"
    , "end program sqrtPoly" ]
