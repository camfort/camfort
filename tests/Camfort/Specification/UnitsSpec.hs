module Camfort.Specification.UnitsSpec (spec) where

import qualified Data.ByteString.Char8 as B

import Language.Fortran.Parser.Any
import Language.Fortran.ParserMonad (fromRight)
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import Data.Generics.Uniplate.Operations
import Camfort.Analysis.Annotations
import Camfort.Input (readParseSrcDir)
import Camfort.Specification.Parser (runParser)
import Camfort.Specification.Units
import Camfort.Specification.Units.Monad
import Camfort.Specification.Units.Parser hiding (UnitAlias)
import Camfort.Specification.Units.InferenceFrontend
import Camfort.Specification.Units.InferenceBackend
import Camfort.Specification.Units.Environment
import Data.List
import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as M
import GHC.Real
import System.FilePath ((</>))

import Test.Hspec


runUnits litMode pf m = (r, usConstraints state)
  where
    pf' = FA.initAnalysis . fmap mkUnitAnnotation . fmap (const unitAnnotation) $ pf
    uOpts = unitOpts0 { uoDebug = False, uoLiterals = litMode }
    (r, state, _) = runUnitSolver uOpts pf' $ initInference >> m

runUnitInference litMode pf = case r of
  Right vars -> ([ (FA.varName e, u) | e <- declVariables pf'
                                     , u <- maybeToList ((FA.varName e, FA.srcName e) `lookup` vars) ]
                , usConstraints state)
  _          -> ([], usConstraints state)
  where
    pf' = FA.initAnalysis . fmap mkUnitAnnotation . fmap (const unitAnnotation) $ pf
    uOpts = unitOpts0 { uoDebug = False, uoLiterals = litMode }
    (r, state, _) = runUnitSolver uOpts pf' $ initInference >> fmap chooseImplicitNames runInferVariables

declVariables :: F.ProgramFile UA -> [F.Expression UA]
declVariables pf = flip mapMaybe (universeBi pf) $ \ d -> case d of
  F.DeclVariable _ _ v@(F.ExpValue _ _ (F.ValVariable _)) _ _   -> Just v
  F.DeclArray    _ _ v@(F.ExpValue _ _ (F.ValVariable _)) _ _ _ -> Just v
  _                                                             -> Nothing


spec :: Spec
spec = do
  let showClean = show . nub . sort . head . rights . (:[]) . fst
  describe "Unit Inference Frontend" $ do
    describe "Literal Mode" $ do
      it "litTest1 Mixed" $ do
        (fromJust (head (rights [fst (runUnits LitMixed litTest1 runInconsistentConstraints)]))) `shouldSatisfy`
          any (conParamEq (ConEq (UnitVar ("k", "k")) (UnitMul (UnitVar ("j", "j")) (UnitVar ("j", "j")))))
      it "litTest1 Poly" $ do
        (fromJust (head (rights [fst (runUnits LitPoly litTest1 runInconsistentConstraints)]))) `shouldSatisfy`
          any (conParamEq (ConEq (UnitVar ("k", "k")) (UnitMul (UnitVar ("j", "j")) (UnitVar ("j", "j")))))
      it "litTest1 Unitless" $ do
        (fromJust (head (rights [fst (runUnits LitUnitless litTest1 runInconsistentConstraints)]))) `shouldSatisfy`
          any (conParamEq (ConEq UnitlessLit (UnitVar ("j", "j"))))
      it "Polymorphic non-zero literal is not OK" $ do
        head (rights [fst (runUnits LitMixed inconsist1 runInconsistentConstraints)]) `shouldSatisfy` isJust

    describe "Polymorphic functions" $ do
      it "squarePoly1" $ do
        showClean (runUnits LitMixed squarePoly1 (fmap chooseImplicitNames runInferVariables)) `shouldBe`
          "[((\"a\",\"a\"),m),((\"b\",\"b\"),s),((\"m\",\"m\"),'b),((\"n\",\"n\"),'a),((\"square\",\"square\"),('a)**2),((\"squarep\",\"squarep\"),('b)**2),((\"x\",\"x\"),m**2),((\"y\",\"y\"),s**2)]"
    describe "Recursive functions" $ do
      it "Recursive Addition is OK" $ do
        showClean (runUnits LitMixed recursive1 (fmap chooseImplicitNames runInferVariables)) `shouldBe`
          "[((\"b\",\"b\"),'a),((\"n\",\"n\"),1),((\"r\",\"r\"),'a),((\"x\",\"x\"),1),((\"y\",\"y\"),m),((\"z\",\"z\"),m)]"

    describe "Recursive functions" $ do
      it "Recursive Multiplication is not OK" $ do
        (fromJust (head (rights [fst (runUnits LitMixed recursive2 runInconsistentConstraints)]))) `shouldSatisfy`
          any (conParamEq (ConEq (UnitParamPosAbs (("recur", "recur"), 0)) (UnitParamPosAbs (("recur", "recur"), 2))))

    describe "Explicitly annotated parametric polymorphic unit variables" $ do
      it "inside-outside" $ do
        showClean (runUnits LitMixed insideOutside runInferVariables) `shouldBe`
          "[((\"inside\",\"inside\"),('a)**2),((\"k\",\"k\"),'a),((\"m\",\"m\"),('a)**2),((\"outside\",\"outside\"),('a)**2),((\"x\",\"x\"),'a),((\"y\",\"y\"),'a)]"
      it "eapVarScope" $ do
        show (sort (fst (runUnitInference LitMixed eapVarScope))) `shouldBe`
          "[(\"f\",('a)**3),(\"g\",'a),(\"j\",'a),(\"k\",('a)**3),(\"x\",'a),(\"y\",'a)]"
      it "eapVarApp" $ do
        show (sort (fst (runUnitInference LitMixed eapVarApp))) `shouldBe`
          "[(\"f\",('a)**2),(\"fj\",'a),(\"fk\",('a)**2),(\"fl\",('a)**4),(\"fx\",'a),(\"g\",'b),(\"gm\",'b),(\"gn\",'b),(\"gx\",'b),(\"h\",m**2),(\"hx\",m),(\"hy\",m**2)]"

    describe "Implicit parametric polymorphic unit variables" $ do
      it "inferPoly1" $ do
        show (sort (fst (runUnitInference LitMixed inferPoly1))) `shouldBe`
          "[(\"fst\",'a),(\"id\",'c),(\"snd\",'d),(\"sqr\",('f)**2),(\"x1\",'c),(\"x2\",'f),(\"x3\",'a),(\"x4\",'e),(\"y3\",'b),(\"y4\",'d)]"

    describe "Intrinsic functions" $ do
      it "sqrtPoly" $ do
        show (sort (fst (runUnitInference LitMixed sqrtPoly))) `shouldBe`
          "[(\"a\",m**2),(\"b\",s**4),(\"c\",j**2),(\"n\",'a),(\"x\",m),(\"y\",s),(\"z\",j)]"

  describe "Unit Inference Backend" $ do
    describe "Flatten constraints" $ do
      it "testCons1" $ do
        flattenConstraints testCons1 `shouldBe` testCons1_flattened
    describe "Shift terms" $ do
      it "testCons1" $ do
        map shiftTerms (flattenConstraints testCons1) `shouldBe` testCons1_shifted
      it "testCons2" $ do
        map shiftTerms (flattenConstraints testCons2) `shouldBe` testCons2_shifted
      it "testCons3" $ do
        map shiftTerms (flattenConstraints testCons3) `shouldBe` testCons3_shifted
    describe "Consistency" $ do
      it "testCons1" $ do
        inconsistentConstraints testCons1 `shouldBe` Just [ConEq (UnitName "kg") (UnitName "m")]
      it "testCons2" $ do
        inconsistentConstraints testCons2 `shouldBe` Nothing
      it "testCons3" $ do
        inconsistentConstraints testCons3 `shouldBe` Nothing
    describe "Critical Variables" $ do
      it "testCons2" $ do
        criticalVariables testCons2 `shouldSatisfy` null
      it "testCons3" $ do
        criticalVariables testCons3 `shouldBe` [UnitVar ("c", "c"), UnitVar ("e", "e")]
      it "testCons4" $ do
        criticalVariables testCons4 `shouldBe` [UnitVar ("simple2_a22", "simple2_a22")]
      it "testCons5" $ do
        criticalVariables testCons5 `shouldSatisfy` null
    describe "Infer Variables" $ do
      it "testCons5" $ do
        show (inferVariables testCons5) `shouldBe` show testCons5_infer
    describe "Check that (restricted) double to ratios is consistent" $ do
      it "test all in -10/-10 ... 10/10, apart from /0" $
        do and [testDoubleToRationalSubset x y | x <- [-10..10], y <- [-10..10]]

    describe "fixtures integration tests" $
      describe "units-check" $
        it "reports (simple) inconsistent units" $
           "example-inconsist-1.f90" `unitsCheckReportIs` exampleInconsist1CheckReport

exampleInconsist1CheckReport :: String
exampleInconsist1CheckReport =
  "\ntests/fixtures/Specification/Units/example-inconsist-1.f90: Inconsistent:\n\
  \ - at 7:7: 'z' should have unit 's'\n\
  \ - at 7:7: Units 's' and 'm' should be equal\n\n\n\
  \(7:7)-(7:11): s === m\n\
  \(7:3)-(7:11): unit_of(z) === s\n\
  \(1:1)-(8:19): unit_of(z) === s && s === m\n"

fixturesDir :: String
fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Units"

-- | Assert that the report of performing units checking on a file is as expected.
unitsCheckReportIs :: String -> String -> Expectation
fileName `unitsCheckReportIs` expectedReport = do
  let file = fixturesDir </> fileName
  [(f,_,pf)] <- readParseSrcDir file []
  checkUnits uOpts (f,pf) `shouldBe` expectedReport
  where uOpts = unitOpts0 { uoDebug = False, uoLiterals = LitMixed }

--------------------------------------------------

testCons1 = [ ConEq (UnitName "kg") (UnitName "m")
            , ConEq (UnitVar ("x", "x")) (UnitName "m")
            , ConEq (UnitVar ("y", "y")) (UnitName "kg")]

testCons1_flattened = [([UnitPow (UnitName "kg") 1.0],[UnitPow (UnitName "m") 1.0])
                      ,([UnitPow (UnitVar ("x", "x")) 1.0],[UnitPow (UnitName "m") 1.0])
                      ,([UnitPow (UnitVar ("y", "y")) 1.0],[UnitPow (UnitName "kg") 1.0])]

testCons1_shifted = [([],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "kg") (-1.0)])
                    ,([UnitPow (UnitVar ("x", "x")) 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (UnitVar ("y", "y")) 1.0],[UnitPow (UnitName "kg") 1.0])]

--------------------------------------------------

testCons2 = [ConEq (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0))) (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0)))
            ,ConEq (UnitName "m") (UnitMul (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0))) (UnitName "s"))
            ,ConEq (UnitAlias "accel") (UnitMul (UnitName "m") (UnitPow (UnitParamPosUse (("simple1_sqr6", "sqr"),0,0)) (-1.0)))
            ,ConEq (UnitName "s") (UnitParamPosUse (("simple1_sqr6", "sqr"),1,0))
            ,ConEq (UnitVar ("simple1_a5", "simple1_a5")) (UnitAlias "accel")
            ,ConEq (UnitVar ("simple1_t4", "simple1_t4")) (UnitName "s")
            ,ConEq (UnitVar ("simple1_v3", "simple1_v3")) (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0)))
            ,ConEq (UnitVar ("simple1_x1", "simple1_x1")) (UnitName "m")
            ,ConEq (UnitVar ("simple1_y2", "simple1_y2")) (UnitName "m")
            ,ConEq (UnitParamPosUse (("simple1_sqr6","sqr"),0,0)) (UnitParamPosUse (("simple1_mul7","mul"),0,1))
            ,ConEq (UnitParamPosUse (("simple1_sqr6","sqr"),1,0)) (UnitParamPosUse (("simple1_mul7","mul"),1,1))
            ,ConEq (UnitParamPosUse (("simple1_sqr6","sqr"),1,0)) (UnitParamPosUse (("simple1_mul7","mul"),2,1))
            ,ConEq (UnitParamPosUse (("simple1_mul7","mul"),0,1)) (UnitMul (UnitParamPosUse (("simple1_mul7","mul"),1,1)) (UnitParamPosUse (("simple1_mul7","mul"),2,1)))
            ,ConEq (UnitAlias "accel") (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-2.0)))]

testCons2_shifted = [([],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "s") (-1.0),UnitPow (UnitName "m") (-1.0),UnitPow (UnitName "s") 1.0])
                    ,([],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "m") (-1.0)])
                    ,([UnitPow (UnitAlias "accel") 1.0,UnitPow (UnitParamPosUse (("simple1_sqr6","sqr"),0,0)) 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (UnitParamPosUse (("simple1_sqr6","sqr"),1,0)) (-1.0)],[UnitPow (UnitName "s") (-1.0)])
                    ,([UnitPow (UnitVar ("simple1_a5", "simple1_a5")) 1.0,UnitPow (UnitAlias "accel") (-1.0)],[])
                    ,([UnitPow (UnitVar ("simple1_t4", "simple1_t4")) 1.0],[UnitPow (UnitName "s") 1.0])
                    ,([UnitPow (UnitVar ("simple1_v3", "simple1_v3")) 1.0],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "s") (-1.0)])
                    ,([UnitPow (UnitVar ("simple1_x1", "simple1_x1")) 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (UnitVar ("simple1_y2", "simple1_y2")) 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (UnitParamPosUse (("simple1_sqr6","sqr"),0,0)) 1.0,UnitPow (UnitParamPosUse (("simple1_mul7","mul"),0,1)) (-1.0)],[])
                    ,([UnitPow (UnitParamPosUse (("simple1_sqr6","sqr"),1,0)) 1.0,UnitPow (UnitParamPosUse (("simple1_mul7","mul"),1,1)) (-1.0)],[])
                    ,([UnitPow (UnitParamPosUse (("simple1_sqr6","sqr"),1,0)) 1.0,UnitPow (UnitParamPosUse (("simple1_mul7","mul"),2,1)) (-1.0)],[])
                    ,([UnitPow (UnitParamPosUse (("simple1_mul7","mul"),0,1)) 1.0,UnitPow (UnitParamPosUse (("simple1_mul7","mul"),1,1)) (-1.0),UnitPow (UnitParamPosUse (("simple1_mul7","mul"),2,1)) (-1.0)],[])
                    ,([UnitPow (UnitAlias "accel") 1.0],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "s") (-2.0)])]

testCons3 = [ ConEq (UnitVar ("a", "a")) (UnitVar ("e", "e"))
            , ConEq (UnitVar ("a", "a")) (UnitMul (UnitVar ("b", "b")) (UnitMul (UnitVar ("c", "c")) (UnitVar ("d", "d"))))
            , ConEq (UnitVar ("d", "d")) (UnitName "m") ]

testCons3_shifted = [([UnitPow (UnitVar ("a", "a")) 1.0,UnitPow (UnitVar ("e", "e")) (-1.0)],[])
                    ,([UnitPow (UnitVar ("a", "a")) 1.0,UnitPow (UnitVar ("b", "b")) (-1.0),UnitPow (UnitVar ("c", "c")) (-1.0),UnitPow (UnitVar ("d", "d")) (-1.0)],[])
                    ,([UnitPow (UnitVar ("d", "d")) 1.0],[UnitPow (UnitName "m") 1.0])]

testCons4 = [ConEq (UnitVar ("simple2_a11", "simple2_a11")) (UnitParamPosUse (("simple2_sqr3","sqr"),0,0))
            ,ConEq (UnitVar ("simple2_a22", "simple2_a22")) (UnitParamPosUse (("simple2_sqr3","sqr"),1,0))
            ,ConEq (UnitVar ("simple2_a11", "simple2_a11")) (UnitVar ("simple2_a11", "simple2_a11"))
            ,ConEq (UnitVar ("simple2_a22", "simple2_a22")) (UnitVar ("simple2_a22", "simple2_a22"))
            ,ConEq (UnitParamPosUse (("simple2_sqr3","sqr"),0,0)) (UnitMul (UnitParamPosUse (("simple2_sqr3","sqr"),1,0)) (UnitParamPosUse (("simple2_sqr3","sqr"),1,0)))]

testCons5 = [ConEq (UnitVar ("simple2_a11", "simple2_a11")) (UnitParamPosUse (("simple2_sqr3","sqr"),0,0))
            ,ConEq (UnitAlias "accel") (UnitParamPosUse (("simple2_sqr3","sqr"),1,0))
            ,ConEq (UnitVar ("simple2_a11", "simple2_a11")) (UnitVar ("simple2_a11", "simple2_a11"))
            ,ConEq (UnitVar ("simple2_a22", "simple2_a22")) (UnitAlias "accel")
            ,ConEq (UnitParamPosUse (("simple2_sqr3","sqr"),0,0)) (UnitMul (UnitParamPosUse (("simple2_sqr3","sqr"),1,0)) (UnitParamPosUse (("simple2_sqr3","sqr"),1,0)))
            ,ConEq (UnitAlias "accel") (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-2.0)))]

testCons5_infer = [(("simple2_a11", "simple2_a11"),UnitMul (UnitPow (UnitName "m") 2.0) (UnitPow (UnitName "s") (-4.0)))
                  ,(("simple2_a22", "simple2_a22"),UnitMul (UnitPow (UnitName "m") 1.0) (UnitPow (UnitName "s") (-2.0)))]

testDoubleToRationalSubset :: Integer -> Integer -> Bool
testDoubleToRationalSubset x y =
    if x <= 10 && y <= 10 && x >= -10 && y >= -10 && y /= 0
    then doubleToRationalSubset (fromIntegral x / fromIntegral y) == Just (x % y)
    else True
--------------------------------------------------

litTest1 = flip fortranParser' "litTest1.f90" . B.pack $ unlines
    [ "program main"
    , "  != unit(a) :: x"
    , "  real :: x, j, k"
    , ""
    , "  j = 1 + 1"
    , "  k = j * j"
    , "  x = x + k"
    , "  x = x * j ! inconsistent"
    , "end program main" ]

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

recursive2 = flip fortranParser' "recursive2.f90" . B.pack $ unlines
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
    , "       r = b * recur(n - 1, b) ! inconsistent"
    , "    end if"
    , "  end function recur"
    , "end program main" ]

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

-- This should be inconsistent because of the use of the literal "10"
-- in the parametric polymorphic function sqr.
inconsist1 = flip fortranParser' "inconsist1.f90" . B.pack $ unlines
    [ "program inconsist1"
    , "  implicit none"
    , "  real :: a, b"
    , "  != unit(m) :: x"
    , "  real :: x = 1"
    , "  != unit(s) :: t"
    , "  real :: t = 2"
    , "  a = sqr(x) "
    , "  b = sqr(t)"
    , "  contains "
    , "  real function sqr(y)"
    , "    real :: y"
    , "    real :: z = 10"
    , "    sqr = y * y + z"
    , "  end function"
    , "end program inconsist1" ]

-- Test intrinsic function sqrt()
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

fortranParser' = \x -> fromRight . (fortranParser x)
