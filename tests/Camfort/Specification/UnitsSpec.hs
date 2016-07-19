{-# LANGUAGE ImplicitParams #-}

module Camfort.Specification.UnitsSpec (spec) where

import qualified Data.ByteString.Char8 as B

import Camfort.Input
import Camfort.Functionality
import Camfort.Output
import Camfort.Specification.Units
import Camfort.Specification.Units.InferenceBackend
import Camfort.Specification.Units.Environment
import Data.List
import Data.Maybe
import qualified Data.Array as A
import qualified Numeric.LinearAlgebra as H
import Numeric.LinearAlgebra (
    atIndex, (<>), (><), rank, (?), toLists, toList, fromLists, fromList, rows, cols,
    takeRows, takeColumns, dropRows, dropColumns, subMatrix, diag, build, fromBlocks,
    ident, flatten, lu, dispf, Matrix
  )

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
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
        criticalVariables testCons3 `shouldBe` [UnitVar "c",UnitVar "e"]
      it "testCons4" $ do
        criticalVariables testCons4 `shouldBe` [UnitVar "simple2_a22"]
      it "testCons5" $ do
        criticalVariables testCons5 `shouldSatisfy` null
    describe "Infer Variables" $ do
      it "testCons5" $ do
        inferVariables testCons5 `shouldBe` testCons5_infer

-- describe "Unit specifications" $ do
--   describe "Integration tests of infer and synthesise" integration


integration = do
   doIntegration 0 "ex1.f90"
      "ex1.f90: Added 0 annotations\n\
      \ex1.f90: checked/inferred 9 user variables\n"

   doIntegration 1 "ex2.f90"
      "ex2.f90: Added 1 annotations\n\
      \ex2.f90: checked/inferred 4 user variables\n"

   doIntegration 2 "ex3.f90"
      "ex3.f90: Added 3 annotations\n\
      \ex3.f90: checked/inferred 6 user variables\n"

   doIntegration 2 "param.f90"
      "param.f90: Added 2 annotations\n\
      \param.f90: checked/inferred 5 user variables\n"

doIntegration c fname expInfer = do
   let file = "tests/Camfort/Specification/Units/" ++ fname
   ps <- runIO $ readForparseSrcDir file []
   let [(fname1, _, program1)] = ps
   let (report, ps') = let ?solver = Custom
                           ?assumeLiterals = Unitless
                       in synthesiseUnits (fname, program1)
   it ("(" ++ show c ++ ") - " ++ fname ++ " infer") $
      report `shouldBe` expInfer

   expect <- runIO $ readFile $ "tests/Camfort/Specification/Units/exp." ++ fname
   it ("(" ++ show c ++ ") - " ++ fname ++ " comapare expected out") $
       (B.unpack $ mkOutputText ("exp." ++ fname) (head $ mkOutputFileForpar ps [ps']))
          `shouldBe` expect

--------------------------------------------------

testCons1 = [ ConEq (UnitName "kg") (UnitName "m")
            , ConEq (UnitVar "x") (UnitName "m")
            , ConEq (UnitVar "y") (UnitName "kg")]

testCons1_flattened = [([UnitPow (UnitName "kg") 1.0],[UnitPow (UnitName "m") 1.0])
                      ,([UnitPow (UnitVar "x") 1.0],[UnitPow (UnitName "m") 1.0])
                      ,([UnitPow (UnitVar "y") 1.0],[UnitPow (UnitName "kg") 1.0])]

testCons1_shifted = [([],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "kg") (-1.0)])
                    ,([UnitPow (UnitVar "x") 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (UnitVar "y") 1.0],[UnitPow (UnitName "kg") 1.0])]

--------------------------------------------------

testCons2 = [ConEq (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0))) (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0)))
            ,ConEq (UnitName "m") (UnitMul (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0))) (UnitName "s"))
            ,ConEq (UnitAlias "accel") (UnitMul (UnitName "m") (UnitPow (UnitParamUse ("simple1_sqr6",0,0)) (-1.0)))
            ,ConEq (UnitName "s") (UnitParamUse ("simple1_sqr6",1,0))
            ,ConEq (UnitVar "simple1_a5") (UnitAlias "accel")
            ,ConEq (UnitVar "simple1_t4") (UnitName "s")
            ,ConEq (UnitVar "simple1_v3") (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0)))
            ,ConEq (UnitVar "simple1_x1") (UnitName "m")
            ,ConEq (UnitVar "simple1_y2") (UnitName "m")
            ,ConEq (UnitParamUse ("simple1_sqr6",0,0)) (UnitParamUse ("simple1_mul7",0,1))
            ,ConEq (UnitParamUse ("simple1_sqr6",1,0)) (UnitParamUse ("simple1_mul7",1,1))
            ,ConEq (UnitParamUse ("simple1_sqr6",1,0)) (UnitParamUse ("simple1_mul7",2,1))
            ,ConEq (UnitParamUse ("simple1_mul7",0,1)) (UnitMul (UnitParamUse ("simple1_mul7",1,1)) (UnitParamUse ("simple1_mul7",2,1)))
            ,ConEq (UnitAlias "accel") (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-2.0)))]

testCons2_shifted = [([],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "s") (-1.0),UnitPow (UnitName "m") (-1.0),UnitPow (UnitName "s") 1.0])
                    ,([],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "m") (-1.0)])
                    ,([UnitPow (UnitAlias "accel") 1.0,UnitPow (UnitParamUse ("simple1_sqr6",0,0)) 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (UnitParamUse ("simple1_sqr6",1,0)) (-1.0)],[UnitPow (UnitName "s") (-1.0)])
                    ,([UnitPow (UnitVar "simple1_a5") 1.0,UnitPow (UnitAlias "accel") (-1.0)],[])
                    ,([UnitPow (UnitVar "simple1_t4") 1.0],[UnitPow (UnitName "s") 1.0])
                    ,([UnitPow (UnitVar "simple1_v3") 1.0],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "s") (-1.0)])
                    ,([UnitPow (UnitVar "simple1_x1") 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (UnitVar "simple1_y2") 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (UnitParamUse ("simple1_sqr6",0,0)) 1.0,UnitPow (UnitParamUse ("simple1_mul7",0,1)) (-1.0)],[])
                    ,([UnitPow (UnitParamUse ("simple1_sqr6",1,0)) 1.0,UnitPow (UnitParamUse ("simple1_mul7",1,1)) (-1.0)],[])
                    ,([UnitPow (UnitParamUse ("simple1_sqr6",1,0)) 1.0,UnitPow (UnitParamUse ("simple1_mul7",2,1)) (-1.0)],[])
                    ,([UnitPow (UnitParamUse ("simple1_mul7",0,1)) 1.0,UnitPow (UnitParamUse ("simple1_mul7",1,1)) (-1.0),UnitPow (UnitParamUse ("simple1_mul7",2,1)) (-1.0)],[])
                    ,([UnitPow (UnitAlias "accel") 1.0],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "s") (-2.0)])]

testCons3 = [ ConEq (UnitVar "a") (UnitVar "e")
            , ConEq (UnitVar "a") (UnitMul (UnitVar "b") (UnitMul (UnitVar "c") (UnitVar "d")))
            , ConEq (UnitVar "d") (UnitName "m") ]

testCons3_shifted = [([UnitPow (UnitVar "a") 1.0,UnitPow (UnitVar "e") (-1.0)],[])
                    ,([UnitPow (UnitVar "a") 1.0,UnitPow (UnitVar "b") (-1.0),UnitPow (UnitVar "c") (-1.0),UnitPow (UnitVar "d") (-1.0)],[])
                    ,([UnitPow (UnitVar "d") 1.0],[UnitPow (UnitName "m") 1.0])]

testCons4 = [ConEq (UnitVar "simple2_a11") (UnitParamUse ("simple2_sqr3",0,0))
            ,ConEq (UnitVar "simple2_a22") (UnitParamUse ("simple2_sqr3",1,0))
            ,ConEq (UnitVar "simple2_a11") (UnitVar "simple2_a11")
            ,ConEq (UnitVar "simple2_a22") (UnitVar "simple2_a22")
            ,ConEq (UnitParamUse ("simple2_sqr3",0,0)) (UnitMul (UnitParamUse ("simple2_sqr3",1,0)) (UnitParamUse ("simple2_sqr3",1,0)))]

testCons5 = [ConEq (UnitVar "simple2_a11") (UnitParamUse ("simple2_sqr3",0,0))
            ,ConEq (UnitAlias "accel") (UnitParamUse ("simple2_sqr3",1,0))
            ,ConEq (UnitVar "simple2_a11") (UnitVar "simple2_a11")
            ,ConEq (UnitVar "simple2_a22") (UnitAlias "accel")
            ,ConEq (UnitParamUse ("simple2_sqr3",0,0)) (UnitMul (UnitParamUse ("simple2_sqr3",1,0)) (UnitParamUse ("simple2_sqr3",1,0)))
            ,ConEq (UnitAlias "accel") (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-2.0)))]

testCons5_infer = [("simple2_a11",UnitMul (UnitPow (UnitName "m") 2.0) (UnitPow (UnitName "s") (-4.0)))
                  ,("simple2_a22",UnitMul (UnitPow (UnitName "m") 1.0) (UnitPow (UnitName "s") (-2.0)))]
