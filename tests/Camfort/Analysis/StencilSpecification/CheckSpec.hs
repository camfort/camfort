module Camfort.Analysis.StencilSpecification.CheckSpec (spec) where

import Camfort.Analysis.StencilSpecification.Check
import qualified Camfort.Analysis.StencilSpecification.Grammar as SYN
import Camfort.Analysis.StencilSpecification.Syntax

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

parseAndConvert x = SYN.specParser x >>= (return . synToAst)

spec :: Test.Spec
spec = describe "Stencils - Check" $ do
  it "parse and convert simple exact stencil (1)" $
      (parseAndConvert "stencil forward(depth=1, dim=1) :: x")
      `shouldBe`
        (Just $ Right $ [(Specification $ Left $
         Exact (Spatial NonLinear [] []
                  (Sum [Product [Forward 1 1]])) , ["x"])])

  it "parse and convert simple exact stencil (2)" $
      (parseAndConvert "stencil forward(depth=1, dim=1) :: x, y, z")
      `shouldBe`
        (Just $ Right $ [(Specification $ Left $
         Exact (Spatial NonLinear [] []
                  (Sum [Product [Forward 1 1]])) , ["x","y","z"])])

  it "parse and convert simple upper bounded stencil (3)" $
      (parseAndConvert "stencil atMost, forward(depth=1, dim=1) :: x")
      `shouldBe`
        (Just $ Right $ [(Specification $ Left $
         Bound Nothing (Just $ Spatial NonLinear [] []
                  (Sum [Product [Forward 1 1]])) , ["x"])])

  it "parse and convert simple lower bounded stencil (4)" $
      (parseAndConvert "stencil atLeast, backward(depth=2, dim=1) :: x")
      `shouldBe`
        (Just $ Right $ [(Specification $ Left $
         Bound (Just $ Spatial NonLinear [] []
                  (Sum [Product [Backward 2 1]])) Nothing, ["x"])])

  it "parse and convert modified bounded stencil (4)" $
      (parseAndConvert "stencil reflexive(dims=1), irreflexive(dims=2), centered(depth=1, dim=3) :: x")
      `shouldBe`
        (Just $ Right $ [(Specification $ Left $
         Exact (Spatial NonLinear [2] [1]
                  (Sum [Product [Centered 1 3]])), ["x"])])

  it "parse and convert stencil requiring distribution" $
      (parseAndConvert "stencil atLeast, reflexive(dims=1,2), readOnce, (forward(depth=1, dim=1) * ((centered(depth=1, dim=2)) + backward(depth=3, dim=4))) :: frob")
      `shouldBe`
        (Just $ Right $ [(Specification $ Left $
         Bound (Just $ Spatial Linear [] [1,2]
                  (Sum [Product [Forward 1 1, Centered 1 2],
                        Product [Forward 1 1, Backward 3 4]])) Nothing
                        , ["frob"])])
