{-# LANGUAGE ImplicitParams #-}

module Camfort.Specification.Stencils.CheckSpec (spec) where

import Camfort.Analysis.CommentAnnotator
import Camfort.Specification.Stencils.CheckBackend
import Camfort.Specification.Stencils.CheckFrontend
import qualified Camfort.Specification.Stencils.Grammar as SYN
import Camfort.Specification.Stencils.Syntax

import Test.Hspec

promoteErrors :: Either String x -> Either AnnotationParseError x
promoteErrors (Left x)  = Left (ProbablyAnnotation x)
promoteErrors (Right x) = Right x

parseAndConvert x =
    let ?renv = []
    in SYN.specParser x >>= (promoteErrors . synToAst)
extract (Right (Right [(_, s)])) = s

spec :: Spec
spec =
  describe "Stencils - Check" $ do
    describe "Parsing comments into internal rep" $ do
      it "parse and convert simple exact stencil (1)" $
          parseAndConvert "= stencil forward(depth=1, dim=1) :: x"
          `shouldBe`
            (Right $ Right [(["x"], Specification $
             Multiple $ Exact (Spatial (Sum [Product [Forward 1 1 True]])))])

      it "parse and convert simple exact stencil (2)" $
          parseAndConvert "= stencil forward(depth=1, dim=1) :: x, y, z"
          `shouldBe`
            (Right $ Right [(["x","y","z"], Specification $
             Multiple $ Exact (Spatial (Sum [Product [Forward 1 1 True]])))])

      it "parse and convert simple exact stencil with nonpointed (2a)" $
          parseAndConvert "= stencil centered(depth=1, dim=2, nonpointed) :: x, y, z"
          `shouldBe`
            (Right $ Right [(["x","y","z"], Specification $
             Multiple $ Exact (Spatial (Sum [Product [Centered 1 2 False]])))])

      it "parse and convert simple upper bounded stencil (3)" $
          parseAndConvert "= stencil atmost, forward(depth=1, dim=1) :: x"
          `shouldBe`
            (Right $ Right [(["x"], Specification $
             Multiple $ Bound Nothing (Just $ Spatial
                      (Sum [Product [Forward 1 1 True]])))])

      it "parse and convert simple lower bounded stencil (4)" $
          parseAndConvert "= stencil atleast, backward(depth=2, dim=1) :: x"
          `shouldBe`
            (Right $ Right [(["x"], Specification $
             Multiple $ Bound (Just $ Spatial
                      (Sum [Product [Backward 2 1 True]])) Nothing)])

      it "parse and convert stencil requiring distribution (5)" $
          parseAndConvert "= stencil atleast, readonce, (forward(depth=1, dim=1) * ((centered(depth=1, dim=2)) + backward(depth=3, dim=4))) :: frob"
          `shouldBe`
            (Right $ Right [(["frob"], Specification $
             Single $ Bound (Just $ Spatial
                      (Sum [Product [Forward 1 1 True, Centered 1 2 True],
                            Product [Forward 1 1 True, Backward 3 4 True]])) Nothing)])
