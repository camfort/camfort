{-# LANGUAGE ImplicitParams #-}

module Camfort.Specification.Stencils.CheckSpec (spec) where

import Camfort.Analysis.CommentAnnotator
import Camfort.Specification.Stencils.CheckBackend
import qualified Camfort.Specification.Stencils.Grammar as SYN
import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.Syntax

import Test.Hspec

parseAndConvert x =
    let ?renv = []
    in case SYN.specParser x of
         Left  _  -> error "received stencil with invalid syntax in test"
         Right v  -> synToAst v

spec :: Spec
spec =
  describe "Stencils - Check" $ do
    describe "Parsing comments into internal rep" $ do
      it "parse and convert simple exact stencil (1)" $
          parseAndConvert "= stencil forward(depth=1, dim=1) :: x"
          `shouldBe`
            (Right $ Right [(["x"], Specification
             (Mult $ Exact (Spatial (Sum [Product [Forward 1 1 True]]))) True)])

      it "parse and convert simple exact stencil (2)" $
          parseAndConvert "= stencil forward(depth=1, dim=1) :: x, y, z"
          `shouldBe`
            (Right $ Right [(["x","y","z"], Specification
             (Mult $ Exact (Spatial (Sum [Product [Forward 1 1 True]]))) True)])

      it "parse and convert simple exact access spec (2)" $
          parseAndConvert "= access forward(depth=1, dim=1) :: x, y, z"
          `shouldBe`
            (Right $ Right [(["x","y","z"], Specification
             (Mult $ Exact (Spatial (Sum [Product [Forward 1 1 True]]))) False)])

      it "parse and convert simple exact stencil with nonpointed (2a)" $
          parseAndConvert "= stencil centered(depth=1, dim=2, nonpointed) :: x, y, z"
          `shouldBe`
            (Right $ Right [(["x","y","z"], Specification
             (Mult $ Exact (Spatial (Sum [Product [Centered 1 2 False]]))) True)])

      it "parse and convert simple upper bounded stencil (3)" $
          parseAndConvert "= stencil atmost, forward(depth=1, dim=1) :: x"
          `shouldBe`
            (Right $ Right [(["x"], Specification
             (Mult $ Bound Nothing (Just $ Spatial
                      (Sum [Product [Forward 1 1 True]]))) True)])

      it "parse and convert simple upper bounded access spec (3)" $
          parseAndConvert "= access atmost, forward(depth=1, dim=1) :: x"
          `shouldBe`
            (Right $ Right [(["x"], Specification
             (Mult $ Bound Nothing (Just $ Spatial
                      (Sum [Product [Forward 1 1 True]]))) False)])

      it "parse and convert simple lower bounded stencil (4)" $
          parseAndConvert "= stencil atleast, backward(depth=2, dim=1) :: x"
          `shouldBe`
            (Right $ Right [(["x"], Specification
             (Mult $ Bound (Just $ Spatial
                      (Sum [Product [Backward 2 1 True]])) Nothing) True)])

      it "parse and convert stencil requiring distribution (5)" $
          parseAndConvert "= stencil readonce, atleast, (forward(depth=1, dim=1) * ((centered(depth=1, dim=2)) + backward(depth=3, dim=4))) :: frob"
          `shouldBe`
            (Right $ Right [(["frob"], Specification
             (Once $ Bound (Just $ Spatial
                      (Sum [Product [Forward 1 1 True, Centered 1 2 True],
                            Product [Forward 1 1 True, Backward 3 4 True]])) Nothing) True)])

      it "rejects stencils with undefined regions" $
         parseAndConvert "= stencil r1 :: a"
         `shouldBe` (Left . regionNotInScope $ "r1")
