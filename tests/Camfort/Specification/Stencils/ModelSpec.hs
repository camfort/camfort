{-# LANGUAGE DataKinds #-}

module Camfort.Specification.Stencils.ModelSpec (spec) where

import Algebra.Lattice
import qualified Data.Set as S
import Data.List.NonEmpty
import qualified Camfort.Helpers.Vec as V

import Camfort.Specification.Stencils.Model

import Test.Hspec

spec :: Spec
spec =
  describe "Model spec" $ do
    describe "unfCompare" $ do
      let reg1 =
            return (V.Cons (IntervHoled 0 2 False) (V.Cons (IntervHoled 0 2 False) V.Nil))
            \/
            return (V.Cons (IntervHoled 0 1 True) (V.Cons (IntervHoled 0 2 False) V.Nil))
      let reg2 = return $ V.Cons (IntervHoled 0 2 True) (V.Cons (IntervHoled 0 2 False) V.Nil)
      it "compares equal regions" $
        unfCompare reg1 reg2 `shouldBe` EQ

      let reg3 =
            reg2 \/ return (V.Cons (IntervHoled 0 3 False) (V.Cons (IntervHoled 0 0 True) V.Nil))
      it "compares greater regions" $
        unfCompare reg3 reg2 `shouldBe` GT

      let reg4 = reg1 \/ return (V.Cons IntervInfinite $ V.Cons IntervInfinite V.Nil)
      it "compares smaller regions" $
        unfCompare reg3 reg4 `shouldBe` LT

      let prod1 = return $ V.Cons (Offsets . S.fromList $ [2,3,5])
                                  (V.Cons (Offsets . S.fromList $ [10, 15]) V.Nil)
      let prod2 = return $ V.Cons (Offsets . S.fromList $ [2,3,4,5])
                                  (V.Cons (Offsets . S.fromList $ [10, 12, 15]) V.Nil)
      it "compare equal offset products" $
        unfCompare prod1 prod2 `shouldBe` LT

      let prod3 = prod1 \/
                  return (V.Cons (Offsets . S.fromList $ [ 4 ])
                                 (V.Cons (Offsets . S.fromList $ [ 10, 12, 15 ]) V.Nil))
                         \/
                  return (V.Cons (Offsets . S.fromList $ [ 2, 3, 4, 5 ])
                                 (V.Cons (Offsets . S.fromList $ [ 12 ]) V.Nil))
      it "compare equal offset products" $
        unfCompare prod3 prod2 `shouldBe` EQ

      let regBack = return $
            V.Cons (IntervHoled (-1) 0 True) (V.Cons IntervInfinite V.Nil)
      let off = return $
            V.Cons (Offsets . S.fromList $ [-1, 0]) (V.Cons SetOfIntegers V.Nil)
      it "compare equal offset and interval" $
        unfCompare regBack off `shouldBe` EQ

      let regFivePoint =
            return (V.Cons (IntervHoled (-1) 1 True)
                           (V.Cons (IntervHoled 0 0 True) V.Nil))
            \/
            return (V.Cons (IntervHoled 0 0 True)
                           (V.Cons (IntervHoled (-1) 1 True) V.Nil))
      let offFivePoint =
            return (V.Cons (Offsets . S.fromList $ [-1])
                           (V.Cons (Offsets . S.fromList $ [0]) V.Nil))
            \/
            return (V.Cons (Offsets . S.fromList $ [0])
                           (V.Cons (Offsets . S.fromList $ [0]) V.Nil))
            \/
            return (V.Cons (Offsets . S.fromList $ [1])
                           (V.Cons (Offsets . S.fromList $ [0]) V.Nil))
            \/
            return (V.Cons (Offsets . S.fromList $ [0])
                           (V.Cons (Offsets . S.fromList $ [-1]) V.Nil))
            \/
            return (V.Cons (Offsets . S.fromList $ [0])
                           (V.Cons (Offsets . S.fromList $ [1]) V.Nil))
      it "compare equal offset and interval" $
        unfCompare regFivePoint offFivePoint `shouldBe` EQ
