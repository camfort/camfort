{-# LANGUAGE ImplicitParams #-}

module Camfort.Specification.Stencils.CheckSpec (spec) where

import Camfort.Analysis.CommentAnnotator
import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.CheckBackend
import Camfort.Specification.Stencils.CheckFrontend
import qualified Camfort.Specification.Stencils.Grammar as SYN
import Camfort.Specification.Stencils.Syntax

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

promoteErrors :: Either String x -> Either AnnotationParseError x
promoteErrors (Left x)  = Left (ProbablyAnnotation x)
promoteErrors (Right x) = Right x

parseAndConvert x = let ?renv = [] in SYN.specParser x >>= (promoteErrors . synToAst)
extract (Right (Right [(_, s)])) = s

spec :: Test.Spec
spec = describe "Stencils - Check" $ do
  it "parse and convert simple exact stencil (1)" $
      (parseAndConvert "= stencil forward(depth=1, dim=1) :: x")
      `shouldBe`
        (Right $ Right $ [(["x"], Specification $
         Exact (Spatial NonLinear (Sum [Product [Forward 1 1 True]])))])

  it "parse and convert simple exact stencil (2)" $
      (parseAndConvert "= stencil forward(depth=1, dim=1) :: x, y, z")
      `shouldBe`
        (Right $ Right $ [(["x","y","z"], Specification $
         Exact (Spatial NonLinear (Sum [Product [Forward 1 1 True]])))])

  it "parse and convert simple exact stencil with irreflexive (2a)" $
      (parseAndConvert "= stencil centered(depth=1, dim=2, irreflexive) :: x, y, z")
      `shouldBe`
        (Right $ Right $ [(["x","y","z"], Specification $
         Exact (Spatial NonLinear (Sum [Product [Centered 1 2 False]])))])

{-
  it "parse and convert simple exact stencil with irreflexive (2b)" $
     let ?dimensionality = 2 in
      ((extract $
        parseAndConvert "= stencil centered(depth=1, dim=2, irreflexive) :: x, y, z")
      `eqByModel`
      (Specification $ Left $ Exact (Spatial NonLinear
                                    (Sum [Product [Centered 1 2 False]]))))
       `shouldBe` True
-}


  it "parse and convert simple upper bounded stencil (3)" $
      (parseAndConvert "= stencil atmost, forward(depth=1, dim=1) :: x")
      `shouldBe`
        (Right $ Right $ [(["x"], Specification $
         Bound Nothing (Just $ Spatial NonLinear
                  (Sum [Product [Forward 1 1 True]])))])

  it "parse and convert simple lower bounded stencil (4)" $
      (parseAndConvert "= stencil atleast, backward(depth=2, dim=1) :: x")
      `shouldBe`
        (Right $ Right $ [(["x"], Specification $
         Bound (Just $ Spatial NonLinear
                  (Sum [Product [Backward 2 1 True]])) Nothing)])

{- This is no longer applicable
  it "parse and convert modified bounded stencil (4)" $
      (parseAndConvert "= stencil reflexive(dims=1), irreflexive(dims=2), centered(depth=1, dim=3) :: x")
      `shouldBe`
        (Right $ Right $ [(["x"], Specification $ Left $
         Exact (Spatial NonLinear [2] (Sum [Product [Centered 1 3]])))])
-}

  it "parse and convert stencil requiring distribution (5)" $
      (parseAndConvert "= stencil atleast, readonce, (forward(depth=1, dim=1) * ((centered(depth=1, dim=2)) + backward(depth=3, dim=4))) :: frob")
      `shouldBe`
        (Right $ Right $ [(["frob"], Specification $
         Bound (Just $ Spatial Linear
                  (Sum [Product [Forward 1 1 True, Centered 1 2 True],
                        Product [Forward 1 1 True, Backward 3 4 True]])) Nothing)])

{-
  it "parse and convert stencil with irreflexivity on a product(6)" $
     let ?dimensionality = 2 in
      ((extract $
        parseAndConvert "= stencil forward(depth=1, dim=2, irreflexive)*backward(depth=1,dim=1) :: x, y, z")
      `eqByModel`
      (Specification $ Left $ Exact (Spatial NonLinear
                                    (Sum [Product [Forward 1 2 False, Backward 1 1 True]]))))
-}
