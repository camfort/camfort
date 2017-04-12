{-# LANGUAGE DataKinds #-}

module Camfort.Specification.Stencils.LatticeModelSpec (spec) where

import Algebra.Lattice
import qualified Data.Set as S
import Data.List.NonEmpty
import qualified Camfort.Helpers.Vec as V

import Camfort.Specification.Stencils.LatticeModel

import Test.Hspec

spec :: Spec
spec =
  describe "Model spec" $ do
    describe "ioCompare" $ do
      let reg1 =
            return (V.Cons (IntervHoled 0 2 False) (V.Cons (IntervHoled 0 2 False) V.Nil))
            \/
            return (V.Cons (IntervHoled 0 1 True) (V.Cons (IntervHoled 0 2 False) V.Nil))
      let reg2 = return $ V.Cons (IntervHoled 0 2 True) (V.Cons (IntervHoled 0 2 False) V.Nil)
      res <- runIO $ ioCompare reg1 reg2

      it "compares equal regions" $
        res `shouldBe` EQ

      let reg3 =
            reg2 \/ return (V.Cons (IntervHoled 0 3 False) (V.Cons (IntervHoled 0 0 True) V.Nil))
      res <- runIO $ ioCompare reg3 reg2
      it "compares greater regions" $
        res `shouldBe` GT

      let reg4 = reg1 \/ return (V.Cons IntervInfinite $ V.Cons IntervInfinite V.Nil)
      res <- runIO $ ioCompare reg3 reg4
      it "compares smaller regions" $
        res `shouldBe` LT
