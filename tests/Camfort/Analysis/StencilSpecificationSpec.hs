{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Camfort.Analysis.StencilSpecificationSpec (spec) where

import Data.List

import Camfort.Helpers.Vec
import Camfort.Analysis.StencilSpecification
import Camfort.Analysis.StencilSpecification.Synthesis
import Camfort.Analysis.StencilSpecification.Model
import Camfort.Analysis.StencilSpecification.Inference
import Camfort.Analysis.StencilSpecification.Syntax hiding (Spec)
import Camfort.Analysis.Annotations
import qualified Forpar.AST as F
import Forpar.Util.Position


import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "Stencils" $ do
    describe "Idempotence of spanBounding" $ do
      it "(0)" $ property $ prop_spanBoundingIdem zero
      it "(1)" $ property $ prop_spanBoundingIdem one
      it "(2)" $ property $ prop_spanBoundingIdem two
      it "(3)" $ property $ prop_spanBoundingIdem three
      it "(4)" $ property $ prop_spanBoundingIdem four

    describe "Associativity of spanBounding" $ do
      it "(0)" $ property $ prop_spanBoundingAssoc zero
      it "(1)" $ property $ prop_spanBoundingAssoc one
      it "(2)" $ property $ prop_spanBoundingAssoc two
      it "(3)" $ property $ prop_spanBoundingAssoc three
      it "(4)" $ property $ prop_spanBoundingAssoc four

    describe "Un-permutable permutations on vectors" $ do
      it "(0)" $ property $ prop_perms_invertable zero
      it "(1)" $ property $ prop_perms_invertable one
      it "(2)" $ property $ prop_perms_invertable two
      it "(3)" $ property $ prop_perms_invertable three
      it "(4)" $ property $ prop_perms_invertable four

    it "sorting on indices" $
      shouldBe (sort [ Cons 1 (Cons 2 (Cons 1 Nil))
                      , Cons 2 (Cons 2 (Cons 3 Nil))
                      , Cons 1 (Cons 3 (Cons 3 Nil))
                      , Cons 0 (Cons 3 (Cons 1 Nil))
                      , Cons 1 (Cons 0 (Cons 2 Nil))
                      , Cons 1 (Cons 1 (Cons 1 Nil))
                      , Cons 2 (Cons 1 (Cons 1 Nil)) ])
                ([ Cons 1 (Cons 1 (Cons 1 Nil))
                , Cons 2 (Cons 1 (Cons 1 Nil))
                , Cons 1 (Cons 2 (Cons 1 Nil))
                , Cons 0 (Cons 3 (Cons 1 Nil))
                , Cons 1 (Cons 0 (Cons 2 Nil))
                , Cons 2 (Cons 2 (Cons 3 Nil))
                , Cons 1 (Cons 3 (Cons 3 Nil))
                ] :: [Vec (S (S (S Z))) Int])

    it "composeRegions (1,0)-(1,0) span and (2,0)-(2,0) span" $
      shouldBe (composeConsecutiveSpans
                  (Cons 1 (Cons 0 Nil), Cons 1 (Cons 0 Nil))
                  (Cons 2 (Cons 0 Nil), Cons 2 (Cons 0 Nil)))
               (Just (Cons 1 (Cons 0 Nil), Cons 2 (Cons 0 Nil)))

    it "composeRegions failing on (1,0)-(2,0) span and (4,0)-(5,0) span" $
      shouldBe (composeConsecutiveSpans
                  (Cons 1 (Cons 0 Nil), Cons 2 (Cons 0 Nil))
                  (Cons 4 (Cons 0 Nil), Cons 5 (Cons 0 Nil)))
               Nothing

    it "composeRegions failing on (1,0)-(2,0) span and (3,1)-(3,1) span" $
      shouldBe (composeConsecutiveSpans
                  (Cons 1 (Cons 0 Nil), Cons 2 (Cons 0 Nil))
                  (Cons 3 (Cons 1 Nil), Cons 3 (Cons 1 Nil)))
               Nothing

    it "five point stencil 2D" $
      -- Sort the expected value for the sake of easy equality
      shouldBe (inferMinimalVectorRegions fivepoint)
               (sort [ (Cons (-1) (Cons 0 Nil), Cons 1 (Cons 0 Nil))
                     , (Cons 0 (Cons (-1) Nil), Cons 0 (Cons 1 Nil)) ])

    it "seven point stencil 3D" $
      shouldBe
        (inferMinimalVectorRegions sevenpoint)
        (sort
           [ (Cons (-1) (Cons 0 (Cons 0 Nil)), Cons 1 (Cons 0 (Cons 0 Nil)))
           , (Cons 0 (Cons (-1) (Cons 0 Nil)), Cons 0 (Cons 1 (Cons 0 Nil)))
           , (Cons 0 (Cons 0 (Cons (-1) Nil)), Cons 0 (Cons 0 (Cons 1 Nil))) ])

    describe "Example stencil inferences" $ do
      it "five point stencil 2D" $
        shouldBe (snd3 $ inferSpecInterval fivepoint)
                 (SpatialSpec [] [] (Union [Product [Symmetric 1 [1, 2]]]))

      it "seven point stencil 2D" $
        shouldBe (snd3 $ inferSpecInterval sevenpoint)
                 (SpatialSpec [] [] (Union [Product [Symmetric 1 [1, 2, 3]]]))

      it "five point stencil 2D with blip" $
        shouldBe (snd3 $ inferSpecInterval fivepointErr)
                 (SpatialSpec [] [] (Union [Product [ Forward 1 [1,2]], Product [Symmetric 1 [1,2]]]))

      it "centered forward" $
        shouldBe (snd3 $ inferSpecInterval centeredFwd)
                 (SpatialSpec [] [] (Union [Product [Forward 1 [1], Symmetric 1 [2]]]))

    describe "Example bounding boxes" $ do
      it "five point stencil 2D" $
        shouldBe (thd3 $ inferSpecInterval fivepoint)
                 (SpatialSpec [] [] (Union [Product [Symmetric 1 [1,2]]]))

      it "seven point stencil 2D" $
        shouldBe (thd3 $ inferSpecInterval sevenpoint)
                 (SpatialSpec [] [] (Union [Product [Symmetric 1 [1,2,3]]]))

      it "five point stencil 2D with blip" $
        shouldBe (thd3 $ inferSpecInterval fivepointErr)
                 (SpatialSpec [] [] (Union [Product [Symmetric 1 [1,2]]]))

      it "centered forward" $
        shouldBe (thd3 $ inferSpecInterval centeredFwd)
                 (SpatialSpec [] [] (Union [Product [Forward 1 [1], Symmetric 1 [2]]]))

    describe "2D stencil verification" $
      mapM_ test2DSpecVariation variations

    describe "3D stencil verification" $
      mapM_ test3DSpecVariation variations3D

    describe "Synthesising indexing expressions from offsets is inverse to extracting offsets
             from indexing expressions; and vice versa" $ do
      it "isomorphism" $ shouldBe $ property $ prop_extract_synth_inverse
 

{- Properties of `spanBoundingBox`: idempotent and associative -}
prop_spanBoundingIdem :: Natural n -> Span (Vec n Int) -> Bool
prop_spanBoundingIdem w x = spanBoundingBox x x == normaliseSpan x

prop_spanBoundingAssoc :: Natural n -> Span (Vec n Int)
                                    -> Span (Vec n Int)
                                    -> Span (Vec n Int) -> Bool
prop_spanBoundingAssoc w x y z =
  (==) (spanBoundingBox x (spanBoundingBox y z))
       (spanBoundingBox (spanBoundingBox x y) z)

{- Permutations that come with 'unpermute' functions are invertable -}
prop_perms_invertable :: (Permutable n) => Natural n -> Vec n Int -> Bool
prop_perms_invertable w xs =
  replicate (fact (lengthV xs)) xs == map (\(xs, f) -> f xs) (permutationsV xs)
  where
    fact 0 = 1
    fact n = n * fact (n - 1)

zero = Zero
one = Succ zero
two = Succ one
three = Succ two
four = Succ three

-- Indices for the 2D five point stencil (deliberately in an odd order)
fivepoint = [Cons (-1) (Cons 0 Nil), Cons 0 (Cons (-1) Nil), Cons 1 (Cons 0 Nil),
             Cons 0 (Cons 1 Nil), Cons 0 (Cons 0 Nil)]
-- Indices for the 3D seven point stencil
sevenpoint = [Cons (-1) (Cons 0 (Cons 0 Nil)), Cons 0 (Cons (-1) (Cons 0 Nil)),
              Cons 0 (Cons 0 (Cons 1 Nil)), Cons 0 (Cons 1 (Cons 0 Nil)),
              Cons 1 (Cons 0 (Cons 0 Nil)), Cons 0 (Cons 0 (Cons (-1) Nil)),
              Cons 0 (Cons 0 (Cons 0 Nil))]
centeredFwd = [Cons 1 (Cons 0 Nil), Cons 0 (Cons 1 Nil), Cons 0 (Cons (-1) Nil),
               Cons 1 (Cons 1 Nil), Cons 0 (Cons 0 Nil), Cons 1 (Cons (-1) Nil)]::[Vec (S (S Z)) Int]

-- Examples of unusal patterns
fivepointErr = [Cons (-1) (Cons 0 Nil), Cons 0 (Cons (-1) Nil), Cons 1 (Cons 0 Nil),
                Cons 0 (Cons 1 Nil), Cons 0 (Cons 0 Nil), Cons 1 (Cons 1 Nil)]::[Vec (S (S Z)) Int]

{- Construct arbtirary vectors and test up to certain sizes -}
instance Arbitrary a => Arbitrary (Vec Z a) where
    arbitrary = return Nil

instance (Arbitrary (Vec n a), Arbitrary a) => Arbitrary (Vec (S n) a) where
    arbitrary = do x  <- arbitrary
                   xs <- arbitrary
                   return $ Cons x xs

test2DSpecVariation (input, expectation) =
    it ("format=" ++ show input) $
      do -- Test inference
         shouldBe (ixCollectionToSpec ["i", "j"] (map fromFormatToExpr input))
           expectation
         -- Test model
         shouldBe (fromList $ model expectation) (sort input)
         

fromFormatToExpr (ri,rj) = [offsetToIxExpr "i" ri, offsetToIxExpr "j" rj]

variations =
        [ ([ (0,0) ], NonLinear $ SpatialSpec [] [ 1, 2 ] (Union [Product []]))
        , ([ (1,0), (0,0) ], NonLinear $ SpatialSpec [] [2] (Union [Product [Forward 1 [ 1 ]]]))
        , ([ (0,1), (0,0) ], NonLinear $ SpatialSpec [] [1] (Union [Product [Forward 1 [ 2 ]]]))
        , ([ (1,1), (0,1), (1,0), (0,0) ], NonLinear $ SpatialSpec [] [] (Union [Product [Forward 1 [ 1, 2 ]]]))
        , ([ (-1,0), (0,0) ], NonLinear $ SpatialSpec [] [2] (Union [Product [Backward 1 [ 1 ]]]))
        , ([ (0,-1), (0,0) ], NonLinear $ SpatialSpec [] [1] (Union [Product [Backward 1 [ 2 ]]]))
        , ([ (-1,-1), (0,-1), (-1,0), (0,0) ], NonLinear $ SpatialSpec [] [] (Union [Product [Backward 1 [ 1, 2 ]]]))
        , ( [ (0,-1), (1,-1), (0,0), (1,0), (1,1), (0,1) ]
          , NonLinear $ SpatialSpec [] [] (Union [Product [ Forward 1 [ 1 ], Symmetric 1 [ 2 ] ] ] ))
         -- Stencil which is non-contiguous from the origin in both directions
        , ([ (0, 1), (1, 1) ], NonLinear $ SpatialSpec [] [] (Union [Product [Forward 1 [ 1 ]]]))
        ]

test3DSpecVariation (input, expectation) =
    it ("format=" ++ show input) $
      -- Test infer
      shouldBe (ixCollectionToSpec ["i", "j", "k"] (map fromFormatToExpr input))
                expectation
       -- Test model
       shouldBe (fromList $ model expectation) (sort input)
  where
    fromFormatToExpr (ri,rj,rk) = [offsetToIxExpr "i" ri, offsetToIxExpr "j" rj, offsetToIxExpr "k" rk]


variations3D =
       [ ([ (-1,0,-1), (0,0,-1), (-1,0,0), (0,0,0) ], (NonLinear $ SpatialSpec [] [2] (Union [Product [Backward 1 [ 1, 3 ]]]))),
         ([ (1,1,0), (0,1,0) ], NonLinear $ SpatialSpec [] [3] (Union [Product [Forward 1 [ 1 ]]])), 
         ([ (-1,4,-1), (0,4,-1), (-1,4,0), (0,4,0) ], (NonLinear $ SpatialSpec [] [] (Union [Product [Backward 1 [ 1, 3 ]]]))) ]

prop_extract_synth_inverse :: F.Name -> Int -> Bool
prop_extract_synth_inverse v o =
     (ixCollectionToSpec [v] (offsetToIxExpr v o) == Just v)

{-
instance Arbitrary Direction where
   arbitrary = do coin <- arbitrary
                  return $ if coin then Fwd else Bwd

instance Arbitrary Spec where
   arbitrary = do coin <- arbitrary
                  if coin
                  then return $ Reflexive
                  else do dir <- arbitrary
                          depth <- choose (0, 10)
                          dim   <- choose (0, 7)
                          sat   <- arbitrary
                          return $ Span depth dim dir sat

incrSpecSat :: Spec -> Spec
incrSpecSat Reflexive = Reflexive
incrSpecSat (Span depth dim dir s) = Span (depth + 1) dim dir True

coalesceConsecutive =
  quickCheck (\spec -> case (plus (NS spec (incrSpecSat spec))) of
                        Just Reflexive -> spec == Reflexive
                        Just (Span sdepth sdim sdir True) -> sdepth == depth (incrSpecSat spec) && sdir == direction spec && sdim == dim spec
                        _ -> False)

normalisedSpecs =
  quickCheck (\specs ->
    all (\(NSpecs sp) ->
     let hd = head sp
     in all (\spec -> (dim spec == dim hd) && (direction spec == direction hd)) sp) (groupByDim specs))

-}
