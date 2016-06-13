{-# LANGUAGE ImplicitParams #-}

module Camfort.Analysis.StencilSpecification.CheckSpec (spec) where

import Camfort.Analysis.CommentAnnotator
import Camfort.Analysis.StencilSpecification.CheckBackend
import Camfort.Analysis.StencilSpecification.CheckFrontend
import qualified Camfort.Analysis.StencilSpecification.Grammar as SYN
import Camfort.Analysis.StencilSpecification.Syntax

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

promoteErrors :: Either String x -> Either AnnotationParseError x
promoteErrors (Left x)  = Left (ProbablyAnnotation x)
promoteErrors (Right x) = Right x

parseAndConvert x = let ?renv = [] in SYN.specParser x >>= (promoteErrors . synToAst)

spec :: Test.Spec
spec = describe "Stencils - Check" $ do
  it "parse and convert simple exact stencil (1)" $
      (parseAndConvert "!= stencil forward(depth=1, dim=1) :: x")
      `shouldBe`
        (Right $ Right $ [(["x"], Specification $ Left $
         Exact (Spatial NonLinear [] []
                  (Sum [Product [Forward 1 1]])))])

  it "parse and convert simple exact stencil (2)" $
      (parseAndConvert "!= stencil forward(depth=1, dim=1) :: x, y, z")
      `shouldBe`
        (Right $ Right $ [(["x","y","z"], Specification $ Left $
         Exact (Spatial NonLinear [] []
                  (Sum [Product [Forward 1 1]])))])

  it "parse and convert simple upper bounded stencil (3)" $
      (parseAndConvert "!= stencil atmost, forward(depth=1, dim=1) :: x")
      `shouldBe`
        (Right $ Right $ [(["x"], Specification $ Left $
         Bound Nothing (Just $ Spatial NonLinear [] []
                  (Sum [Product [Forward 1 1]])))])

  it "parse and convert simple lower bounded stencil (4)" $
      (parseAndConvert "!= stencil atleast, backward(depth=2, dim=1) :: x")
      `shouldBe`
        (Right $ Right $ [(["x"], Specification $ Left $
         Bound (Just $ Spatial NonLinear [] []
                  (Sum [Product [Backward 2 1]])) Nothing)])

  it "parse and convert modified bounded stencil (4)" $
      (parseAndConvert "!= stencil reflexive(dims=1), irreflexive(dims=2), centered(depth=1, dim=3) :: x")
      `shouldBe`
        (Right $ Right $ [(["x"], Specification $ Left $
         Exact (Spatial NonLinear [2] [1]
                  (Sum [Product [Centered 1 3]])))])

  it "parse and convert stencil requiring distribution" $
      (parseAndConvert "!= stencil atleast, reflexive(dims=1,2), readonce, (forward(depth=1, dim=1) * ((centered(depth=1, dim=2)) + backward(depth=3, dim=4))) :: frob")
      `shouldBe`
        (Right $ Right $ [(["frob"], Specification $ Left $
         Bound (Just $ Spatial Linear [] [1,2]
                  (Sum [Product [Forward 1 1, Centered 1 2],
                        Product [Forward 1 1, Backward 3 4]])) Nothing)])
