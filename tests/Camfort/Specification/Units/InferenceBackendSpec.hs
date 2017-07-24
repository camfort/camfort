module Camfort.Specification.Units.InferenceBackendSpec (spec) where

import GHC.Real ((%))

import           Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.InferenceBackend
  ( criticalVariables
  , flattenConstraints
  , inconsistentConstraints
  , inferVariables
  , shiftTerms )

spec :: Test.Spec
spec = do
  describe "Flatten constraints" $
    it "testCons1" $
      flattenConstraints testCons1 `shouldBe` testCons1_flattened
  describe "Shift terms" $ do
    it "testCons1" $
      map shiftTerms (flattenConstraints testCons1) `shouldBe` testCons1_shifted
    it "testCons2" $
      map shiftTerms (flattenConstraints testCons2) `shouldBe` testCons2_shifted
    it "testCons3" $
      map shiftTerms (flattenConstraints testCons3) `shouldBe` testCons3_shifted
  describe "Consistency" $ do
    it "testCons1" $
      inconsistentConstraints testCons1 `shouldBe` Just [ConEq (UnitName "kg") (UnitName "m")]
    it "testCons2" $
      inconsistentConstraints testCons2 `shouldBe` Nothing
    it "testCons3" $
      inconsistentConstraints testCons3 `shouldBe` Nothing
  describe "Critical Variables" $ do
    it "testCons2" $
      criticalVariables testCons2 `shouldSatisfy` null
    it "testCons3" $
      criticalVariables testCons3 `shouldBe` [UnitVar ("c", "c"), UnitVar ("e", "e")]
    it "testCons4" $
      criticalVariables testCons4 `shouldBe` [UnitVar ("simple2_a22", "simple2_a22")]
    it "testCons5" $
      criticalVariables testCons5 `shouldSatisfy` null
  describe "Infer Variables" $
    it "testCons5" $
      show (inferVariables testCons5) `shouldBe` show testCons5_infer
  describe "Check that (restricted) double to ratios is consistent" $
    it "test all in -10/-10 ... 10/10, apart from /0" $
      and [testDoubleToRationalSubset x y | x <- [-10..10], y <- [-10..10]]

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
    not (x <= 10 && y <= 10 && x >= -10 && y >= -10 && y /= 0)
      || doubleToRationalSubset (fromIntegral x / fromIntegral y) == Just (x % y)
