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
        inconsistentConstraints testCons1 `shouldBe` Just [UnitEq (UnitName "kg") (UnitName "m")]
      it "testCons2" $ do
        inconsistentConstraints testCons2 `shouldBe` Nothing
      it "testCons3" $ do
        inconsistentConstraints testCons3 `shouldBe` Nothing
    describe "Critical Variables" $ do
      it "testCons2" $ do
        criticalVariables testCons2 `shouldSatisfy` null
      it "testCons3" $ do
        criticalVariables testCons3 `shouldBe` [Undetermined "c",Undetermined "e"]
      it "testCons4" $ do
        criticalVariables testCons4 `shouldBe` [Undetermined "simple2_a22"]
      it "testCons5" $ do
        criticalVariables testCons5 `shouldSatisfy` null
    describe "Infer Variables" $ do
      it "testCons5" $ do
        inferVariables testCons5 `shouldBe` [("simple2_a11",UnitMul (UnitPow (UnitName "m") 2.0) (UnitPow (UnitName "s") (-4.0)))]

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

testCons1 = [ UnitEq (UnitName "kg") (UnitName "m")
            , UnitEq (Determined "x") (UnitName "m")
            , UnitEq (Determined "y") (UnitName "kg")]

testCons1_flattened = [([UnitPow (UnitName "kg") 1.0],[UnitPow (UnitName "m") 1.0])
                      ,([UnitPow (Determined "x") 1.0],[UnitPow (UnitName "m") 1.0])
                      ,([UnitPow (Determined "y") 1.0],[UnitPow (UnitName "kg") 1.0])]

testCons1_shifted = [([],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "kg") (-1.0)])
                    ,([UnitPow (Determined "x") 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (Determined "y") 1.0],[UnitPow (UnitName "kg") 1.0])]

--------------------------------------------------

testCons2 = [UnitEq (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0))) (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0)))
            ,UnitEq (UnitName "m") (UnitMul (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0))) (UnitName "s"))
            ,UnitEq (UnitAlias "accel") (UnitMul (UnitName "m") (UnitPow (ParametricUse ("simple1_sqr6",0,0)) (-1.0)))
            ,UnitEq (UnitName "s") (ParametricUse ("simple1_sqr6",1,0))
            ,UnitEq (Determined "simple1_a5") (UnitAlias "accel")
            ,UnitEq (Determined "simple1_t4") (UnitName "s")
            ,UnitEq (Determined "simple1_v3") (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-1.0)))
            ,UnitEq (Determined "simple1_x1") (UnitName "m")
            ,UnitEq (Determined "simple1_y2") (UnitName "m")
            ,UnitEq (ParametricUse ("simple1_sqr6",0,0)) (ParametricUse ("simple1_mul7",0,1))
            ,UnitEq (ParametricUse ("simple1_sqr6",1,0)) (ParametricUse ("simple1_mul7",1,1))
            ,UnitEq (ParametricUse ("simple1_sqr6",1,0)) (ParametricUse ("simple1_mul7",2,1))
            ,UnitEq (ParametricUse ("simple1_mul7",0,1)) (UnitMul (ParametricUse ("simple1_mul7",1,1)) (ParametricUse ("simple1_mul7",2,1)))
            ,UnitEq (UnitAlias "accel") (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-2.0)))]

testCons2_shifted = [([],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "s") (-1.0),UnitPow (UnitName "m") (-1.0),UnitPow (UnitName "s") 1.0])
                    ,([],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "m") (-1.0)])
                    ,([UnitPow (UnitAlias "accel") 1.0,UnitPow (ParametricUse ("simple1_sqr6",0,0)) 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (ParametricUse ("simple1_sqr6",1,0)) (-1.0)],[UnitPow (UnitName "s") (-1.0)])
                    ,([UnitPow (Determined "simple1_a5") 1.0,UnitPow (UnitAlias "accel") (-1.0)],[])
                    ,([UnitPow (Determined "simple1_t4") 1.0],[UnitPow (UnitName "s") 1.0])
                    ,([UnitPow (Determined "simple1_v3") 1.0],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "s") (-1.0)])
                    ,([UnitPow (Determined "simple1_x1") 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (Determined "simple1_y2") 1.0],[UnitPow (UnitName "m") 1.0])
                    ,([UnitPow (ParametricUse ("simple1_sqr6",0,0)) 1.0,UnitPow (ParametricUse ("simple1_mul7",0,1)) (-1.0)],[])
                    ,([UnitPow (ParametricUse ("simple1_sqr6",1,0)) 1.0,UnitPow (ParametricUse ("simple1_mul7",1,1)) (-1.0)],[])
                    ,([UnitPow (ParametricUse ("simple1_sqr6",1,0)) 1.0,UnitPow (ParametricUse ("simple1_mul7",2,1)) (-1.0)],[])
                    ,([UnitPow (ParametricUse ("simple1_mul7",0,1)) 1.0,UnitPow (ParametricUse ("simple1_mul7",1,1)) (-1.0),UnitPow (ParametricUse ("simple1_mul7",2,1)) (-1.0)],[])
                    ,([UnitPow (UnitAlias "accel") 1.0],[UnitPow (UnitName "m") 1.0,UnitPow (UnitName "s") (-2.0)])]

testCons3 = [ UnitEq (Undetermined "a") (Undetermined "e")
            , UnitEq (Undetermined "a") (UnitMul (Undetermined "b") (UnitMul (Undetermined "c") (Determined "d")))
            , UnitEq (Determined "d") (UnitName "m") ]

testCons3_shifted = [([UnitPow (Undetermined "a") 1.0,UnitPow (Undetermined "e") (-1.0)],[])
                    ,([UnitPow (Undetermined "a") 1.0,UnitPow (Determined "d") (-1.0),UnitPow (Undetermined "b") (-1.0),UnitPow (Undetermined "c") (-1.0)],[])
                    ,([UnitPow (Determined "d") 1.0],[UnitPow (UnitName "m") 1.0])]

testCons4 = [UnitEq (Undetermined "simple2_a11") (ParametricUse ("simple2_sqr3",0,0))
            ,UnitEq (Undetermined "simple2_a22") (ParametricUse ("simple2_sqr3",1,0))
            ,UnitEq (Determined "simple2_a11") (Undetermined "simple2_a11")
            ,UnitEq (Determined "simple2_a22") (Undetermined "simple2_a22")
            ,UnitEq (ParametricUse ("simple2_sqr3",0,0)) (UnitMul (ParametricUse ("simple2_sqr3",1,0)) (ParametricUse ("simple2_sqr3",1,0)))]

testCons5 = [UnitEq (Undetermined "simple2_a11") (ParametricUse ("simple2_sqr3",0,0))
            ,UnitEq (UnitAlias "accel") (ParametricUse ("simple2_sqr3",1,0))
            ,UnitEq (Determined "simple2_a11") (Undetermined "simple2_a11")
            ,UnitEq (Determined "simple2_a22") (UnitAlias "accel")
            ,UnitEq (ParametricUse ("simple2_sqr3",0,0)) (UnitMul (ParametricUse ("simple2_sqr3",1,0)) (ParametricUse ("simple2_sqr3",1,0)))
            ,UnitEq (UnitAlias "accel") (UnitMul (UnitName "m") (UnitPow (UnitName "s") (-2.0)))]
