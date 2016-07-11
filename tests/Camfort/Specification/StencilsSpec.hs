{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Camfort.Specification.StencilsSpec (spec) where

import Control.Monad.Writer.Strict hiding (Sum, Product)
import Data.List

import Camfort.Functionality
import Camfort.Helpers.Vec
import Camfort.Specification.Stencils
import Camfort.Specification.Stencils.Synthesis
import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.InferenceBackend
import Camfort.Specification.Stencils.InferenceFrontend
import Camfort.Specification.Stencils.Syntax hiding (Spec)
import Camfort.Analysis.Annotations
import qualified Language.Fortran.AST as F
import Language.Fortran.Util.Position

import Data.Map.Strict (toList)
import qualified Data.IntMap as IM
import qualified Data.Set as S

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "Stencils" $ do
    describe "Idempotence of spanBounding" $ do
      it "(0)" $ property $ prop_spanBoundingIdem zeroN
      it "(1)" $ property $ prop_spanBoundingIdem oneN
      it "(2)" $ property $ prop_spanBoundingIdem twoN
      it "(3)" $ property $ prop_spanBoundingIdem threeN
      it "(4)" $ property $ prop_spanBoundingIdem fourN

    describe "Associativity of spanBounding" $ do
      it "(0)" $ property $ prop_spanBoundingAssoc zeroN
      it "(1)" $ property $ prop_spanBoundingAssoc oneN
      it "(2)" $ property $ prop_spanBoundingAssoc twoN
      it "(3)" $ property $ prop_spanBoundingAssoc threeN
      it "(4)" $ property $ prop_spanBoundingAssoc fourN

    describe "Un-permutable permutations on vectors" $ do
      it "(0)" $ property $ prop_perms_invertable zeroN
      it "(1)" $ property $ prop_perms_invertable oneN
      it "(2)" $ property $ prop_perms_invertable twoN
      it "(3)" $ property $ prop_perms_invertable threeN
      it "(4)" $ property $ prop_perms_invertable fourN

    describe "Some checks on containing spans" $ do
      it "(0)" $ containedWithin (Cons 1 (Cons 1 Nil), Cons 2 (Cons 2 Nil))
                          (Cons 0 (Cons 0 Nil), Cons 3 (Cons 3 Nil))
                  `shouldBe` True
      it "(1)" $ containedWithin (Cons 0 (Cons 0 Nil), Cons 3 (Cons 3 Nil))
                          (Cons 1 (Cons 1 Nil), Cons 2 (Cons 2 Nil))
                  `shouldBe` False
      it "(2)" $ containedWithin (Cons 2 (Cons 2 Nil), Cons 2 (Cons 2 Nil))
                          (Cons 1 (Cons 1 Nil), Cons 2 (Cons 2 Nil))
                  `shouldBe` True
      it "(3)" $ containedWithin (Cons 2 (Cons 2 Nil), Cons 3 (Cons 3 Nil))
                          (Cons 1 (Cons 1 Nil), Cons 2 (Cons 2 Nil))
                  `shouldBe` False
      it "(4)" $ containedWithin (Cons 2 Nil, Cons 2 Nil)
                                 (Cons 2 Nil, Cons 2 Nil)
                  `shouldBe` True


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
               ([(Cons 1 (Cons 0 Nil), Cons 2 (Cons 0 Nil))])

    it "composeRegions failing on (1,0)-(2,0) span and (4,0)-(5,0) span" $
      shouldBe (composeConsecutiveSpans
                  (Cons 1 (Cons 0 Nil), Cons 2 (Cons 0 Nil))
                  (Cons 4 (Cons 0 Nil), Cons 5 (Cons 0 Nil)))
               []

    it "composeRegions failing on (1,0)-(2,0) span and (3,1)-(3,1) span" $
      shouldBe (composeConsecutiveSpans
                  (Cons 1 (Cons 0 Nil), Cons 2 (Cons 0 Nil))
                  (Cons 3 (Cons 1 Nil), Cons 3 (Cons 1 Nil)))
               []

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
        (inferFromIndices $ VL fivepoint)
        `shouldBe`
         (exactSp $ Spatial Linear
                     (Sum [ Product [ Centered 0 1 True, Centered 1 2 True]
                          , Product [ Centered 0 2 True, Centered 1 1 True]
                          ]))

      it "seven point stencil 2D" $
        (inferFromIndices $ VL sevenpoint)
        `shouldBe`
          (exactSp $ Spatial Linear
                       (Sum [ Product [ Centered 0 1 True, Centered 0 2 True, Centered 1 3 True]
                            , Product [ Centered 0 1 True, Centered 0 3 True, Centered 1 2 True]
                            , Product [ Centered 0 2 True, Centered 0 3 True, Centered 1 1 True]
                            ]))

      it "five point stencil 2D with blip" $
         (inferFromIndices $ VL fivepointErr)
         `shouldBe`
          (exactSp $ Spatial Linear
                         (Sum [ Product [ Forward 1 1 True, Forward 1 2 True],
                                Product [ Centered 0 1 True, Centered 1 2 True],
                                Product [ Centered 0 2 True, Centered 1 1 True] ]))

      it "centered forward" $
         (inferFromIndices $ VL centeredFwd)
         `shouldBe`
          (exactSp $ Spatial Linear (Sum [ Product [ Forward 1 1 True
                                                  , Centered 1 2 True] ]))

    describe "2D stencil verification" $
      mapM_ (test2DSpecVariation (Neighbour "i" 0) (Neighbour "j" 0)) variations

    describe "2D stencil verification relative" $
      mapM_ (\(a, b, x, y) -> test2DSpecVariation a b (x, y)) variationsRel


    describe "3D stencil verification" $
      mapM_ test3DSpecVariation variations3D

    describe ("Synthesising indexing expressions from offsets is inverse to" ++
              "extracting offsets from indexing expressions; and vice versa") $
      it "isomorphism" $ property prop_extract_synth_inverse

    describe ("Inconsistent induction variable usage tests") $ do
      it "consistent (1) a(i,j) = b(i+1,j+1) + b(i,j)" $
        (indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0, Neighbour "j" 0]
                        [[offsetToIx "i" 1, offsetToIx "j" 1],
                         [offsetToIx "i" 0, offsetToIx "j" 0]])
         `shouldBe` (Just $ Specification $ Left $ Exact
                       (Spatial Linear
                         (Sum [Product [Forward 1 1 False, Forward 1 2 False],
                               Product [Centered 0 1 True, Centered 0 2 True]])))
      it "consistent (2) a(i,c,j) = b(i,j+1) + b(i,j) \
                        \:: forward(depth=1,dim=2)*reflexive(dim=1)" $
        (indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0, Constant (F.ValInteger "0"), Neighbour "j" 0]
                        [[offsetToIx "i" 0, offsetToIx "j" 1],
                         [offsetToIx "i" 0, offsetToIx "j" 0]])
         `shouldBe` (Just $ Specification $ Left $ Exact
                       (Spatial Linear
                         (Sum [Product [Forward 1 2 True, Centered 0 1 True]])))

      it "consistent (3) a(i+1,c,j) = b(j,i+1) + b(j,i) \
                        \:: backward(depth=1,dim=2)*reflexive(dim=1)" $
        (indicesToSpec' ["i", "j"]
                        [Neighbour "i" 1, Constant (F.ValInteger "0"), Neighbour "j" 0]
                        [[offsetToIx "j" 0, offsetToIx "i" 1],
                         [offsetToIx "j" 0, offsetToIx "i" 0]])
         `shouldBe` (Just $ Specification $ Left $ Exact
                       (Spatial Linear
                         (Sum [Product [Backward 1 2 True, Centered 0 1 True]])))

      it "consistent (4) a(i+1,j) = b(0,i+1) + b(0,i) \
                         \:: backward(depth=1,dim=2)" $
        (indicesToSpec' ["i", "j"]
                        [Neighbour "i" 1, Neighbour "j" 0]
                        [[offsetToIx "j" absoluteRep, offsetToIx "i" 1],
                         [offsetToIx "j" absoluteRep, offsetToIx "i" 0]])
         `shouldBe` (Just $ Specification $ Left $ Exact
                       (Spatial Linear
                         (Sum [Product [Backward 1 2 True]])))

      it "consistent (5) a(i) = b(i,i+1) \
                        \:: reflexive(dim=1)*forward(depth=1,dim=2,irreflexive)" $
        (indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0]
                        [[offsetToIx "i" 0, offsetToIx "i" 1]])
         `shouldBe` (Just $ Specification $ Left $ Exact
                       (Spatial Linear
                         (Sum [Product [Forward 1 2 False,
                                        Centered 0 1 True]])))

      it "consistent (6) a(i) = b(i) + b(0) \
                        \:: reflexive(dim=1)" $
        (indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0]
                        [[offsetToIx "i" 0], [offsetToIx "i" absoluteRep]])
         `shouldBe` (Just $ Specification $ Left $ Exact
                       (Spatial Linear
                         (Sum [Product [Centered 0 1 True]])))

      it "inconsistent (1) RHS" $
        (indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0, Neighbour "j" 0]
                        [[offsetToIx "i" 1, offsetToIx "j" 1],
                         [offsetToIx "j" 0, offsetToIx "i" 0]])
         `shouldBe` Nothing

      it "inconsistent (2) RHS to LHS" $
        (indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0]
                        [[offsetToIx "i" 1, offsetToIx "j" 1],
                         [offsetToIx "j" 0, offsetToIx "i" 0]])
         `shouldBe` Nothing

    -------------------------
    -- Some integration tests
    -------------------------

    let file = "tests/Camfort/Specification/Stencils/example2.f"
    program <- runIO $ readForparseSrcDir file []

    describe "integration test on inference for example2.f" $ do
      it "stencil infer" $
         (fst $ callAndSummarise (infer AssignMode) program)
           `shouldBe`
           "\ntests/Camfort/Specification/Stencils/example2.f\n\
            \((24,8),(24,53)) \tstencil readOnce, (reflexive(dim=1))*(centered(depth=1, dim=2)) \
                                     \+ (reflexive(dim=2))*(centered(depth=1, dim=1)) :: a\n\
            \((32,7),(32,26)) \tstencil readOnce, (backward(depth=1, dim=1)) :: a\n\
            \((40,8),(40,62)) \tstencil readOnce, (centered(depth=1, dim=1)) \
                                                \+ (centered(depth=1, dim=2)) :: a\n\
            \((41,8),(41,35)) \tstencil readOnce, (reflexive(dim=1))*(reflexive(dim=2)) :: a\n"

      it "stencil check" $
         (fst $ callAndSummarise (\f p -> (check f p, p)) program)
           `shouldBe`
           "\ntests/Camfort/Specification/Stencils/example2.f\n\
            \((22,12),(22,44)) \tCorrect.\n"

    let file = "tests/Camfort/Specification/Stencils/example3.f"
    program <- runIO $ readForparseSrcDir file []

    describe "integration test on inference for example3.f" $ do
      it "stencil infer" $
         (fst $ callAndSummarise (infer AssignMode) program)
           `shouldBe`
            "\ntests/Camfort/Specification/Stencils/example3.f\n\
             \((15,2),(15,20)) \tstencil readOnce, (reflexive(dim=3)) :: a\n\
             \((20,8),(20,26)) \tstencil readOnce, (reflexive(dim=3)) :: a\n\
             \((23,7),(23,17)) \tstencil readOnce, (reflexive(dim=1)) :: d\n\
             \((24,7),(24,19)) \tstencil readOnce, (reflexive(dim=2)) :: a\n"

    let file = "tests/Camfort/Specification/Stencils/example4.f"
    program <- runIO $ readForparseSrcDir file []

    describe "integration test on inference for example4.f" $ do
      it "stencil infer" $
         (fst $ callAndSummarise (infer AssignMode) program)
           `shouldBe`
            "\ntests/Camfort/Specification/Stencils/example4.f\n\
             \((6,8),(6,33)) \tstencil (reflexive(dim=1)) :: x\n"


exactSp = Specification . Left . Exact

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

zeroN  = Zero
oneN   = Succ zeroN
twoN   = Succ oneN
threeN = Succ twoN
fourN  = Succ threeN

-- Indices for the 2D five point stencil (deliberately in an odd order)
fivepoint = [ Cons (-1) (Cons 0 Nil), Cons 0 (Cons (-1) Nil)
            , Cons 1 (Cons 0 Nil) , Cons 0 (Cons 1 Nil), Cons 0 (Cons 0 Nil)
            ]
-- Indices for the 3D seven point stencil
sevenpoint = [ Cons (-1) (Cons 0 (Cons 0 Nil)), Cons 0 (Cons (-1) (Cons 0 Nil))
             , Cons 0 (Cons 0 (Cons 1 Nil)), Cons 0 (Cons 1 (Cons 0 Nil))
             , Cons 1 (Cons 0 (Cons 0 Nil)), Cons 0 (Cons 0 (Cons (-1) Nil))
             , Cons 0 (Cons 0 (Cons 0 Nil))
             ]
centeredFwd = [ Cons 1 (Cons 0 Nil), Cons 0 (Cons 1 Nil), Cons 0 (Cons (-1) Nil)
              , Cons 1 (Cons 1 Nil), Cons 0 (Cons 0 Nil), Cons 1 (Cons (-1) Nil)
              ] :: [ Vec (S (S Z)) Int ]

-- Examples of unusal patterns
fivepointErr = [ Cons (-1) (Cons 0 Nil)
               , Cons 0 (Cons (-1) Nil)
               , Cons 1 (Cons 0 Nil)
               , Cons 0 (Cons 1 Nil)
               , Cons 0 (Cons 0 Nil)
               , Cons 1 (Cons 1 Nil) ] :: [ Vec (S (S Z)) Int ]

{- Construct arbtirary vectors and test up to certain sizes -}
instance Arbitrary a => Arbitrary (Vec Z a) where
    arbitrary = return Nil

instance (Arbitrary (Vec n a), Arbitrary a) => Arbitrary (Vec (S n) a) where
    arbitrary = do x  <- arbitrary
                   xs <- arbitrary
                   return $ Cons x xs

test2DSpecVariation a b (input, expectation) =
    it ("format=" ++ show input) $ do

       -- Test inference
       (indicesToSpec' ["i", "j"]
                       [a, b]
                       (map fromFormatToIx input))
          `shouldBe` Just expectedSpec
  where
    expectedSpec = Specification . Left $ expectation
    fromFormatToIx [ri,rj] = [ offsetToIx "i" ri, offsetToIx "j" rj ]

indicesToSpec' ivs lhs = fst . runWriter . (indicesToSpec ivmap "a" lhs)
  where ivmap = IM.singleton 0 (S.fromList ivs)

variations =
  [ ( [ [0,0] ]
    , Exact $ Spatial Linear (Sum [Product [ Centered 0 1 True, Centered 0 2 True]])
    )
  , ( [ [1,0] ]
    , Exact $ Spatial Linear (Sum [Product [Forward 1 1 False, Centered 0 2 True]])
    )
  , ( [ [1,0], [0,0], [0,0] ]
    , Exact $ Spatial NonLinear (Sum [Product [Forward 1 1 True, Centered 0 2 True]])
    )
  , ( [ [0,1], [0,0] ]
    , Exact $ Spatial Linear (Sum [Product [Forward 1 2 True, Centered 0 1 True]])
    )
  , ( [ [1,1], [0,1], [1,0], [0,0] ]
    , Exact $ Spatial Linear (Sum [Product [Forward 1 1 True, Forward 1 2 True]])
    )
  , ( [ [-1,0], [0,0] ]
    , Exact $ Spatial Linear (Sum [Product [Backward 1 1 True, Centered 0 2 True]])
    )
  , ( [ [0,-1], [0,0], [0,-1] ]
    , Exact $ Spatial NonLinear (Sum [Product [Backward 1 2 True, Centered 0 1 True]])
    )
  , ( [ [-1,-1], [0,-1], [-1,0], [0,0], [0, -1] ]
    , Exact $ Spatial NonLinear (Sum [Product [Backward 1 1 True, Backward 1 2 True]])
    )
  , ( [ [0,-1], [1,-1], [0,0], [1,0], [1,1], [0,1] ]
    , Exact $ Spatial Linear $ Sum [ Product [ Forward 1 1 True, Centered 1 2 True] ]
    )
   -- Stencil which is non-contiguous in one direction
  , ( [ [0, 4], [1, 4] ]
    , Bound (Just (Spatial Linear (Sum [Product [Forward 1 1 True]])))
            (Just (Spatial Linear (Sum [Product [Forward 1 1 True, Forward 4 2 True]])))
    )
  ]

variationsRel =
  [   -- Stencil which has non-relative indices in one dimension
    (Neighbour "i" 0, Constant (F.ValInteger "0"), [ [0, absoluteRep], [1, absoluteRep] ]
    , Exact $ Spatial Linear (Sum [Product [Forward 1 1 True]])
    )
  , (Neighbour "i" 1, Neighbour "j" 0, [ [0,0] ]
    , Exact $ Spatial Linear (Sum [Product [ Backward 1 1 False, Centered 0 2 True]])
    )
  , (Neighbour "i" 0, Neighbour "j" 1, [ [0,1] ]
    , Exact $ Spatial Linear (Sum [Product [Centered 0 1 True, Centered 0 2 True]])
    )
  , (Neighbour "i" 1, Neighbour "j" (-1), [ [1,0], [0,0], [0,0] ]
    , Exact $ Spatial NonLinear (Sum [Product [Forward 1 2 False, Backward 1 1 True]])
    )
  , (Neighbour "i" 0, Neighbour "j" (-1), [ [0,1], [0,0] ]
    , Exact $ Spatial Linear (Sum [Product [Forward 2 2 False, Centered 0 1 True]])
    )
  -- [0,1] [0,0] [0,-1]
  , (Neighbour "i" 1, Neighbour "j" 0, [ [1,1], [1,0], [1,-1] ]
    , Exact $ Spatial Linear (Sum [Product [Centered 0 1 True, Centered 1 2 True]])
    )
  , (Neighbour "i" 1, Neighbour "j" 0, [ [-2,0], [-1,0] ]
    , Bound (Just (Spatial Linear (Sum [Product [Centered 0 2 True]])))
            (Just (Spatial Linear (Sum [Product [Backward 3 1 True, Centered 0 2 True]]))))

  , (Constant (F.ValInteger "0"), Neighbour "j" 0, [ [absoluteRep,1], [absoluteRep,0], [absoluteRep,-1] ]
    , Exact $ Spatial Linear (Sum [Product [Centered 1 2 True]])
    )
  ]

test3DSpecVariation (input, expectation) =
    it ("format=" ++ show input) $ do

      -- Test inference
      (indicesToSpec' ["i", "j", "k"]
                      [Neighbour "i" 0, Neighbour "j" 0, Neighbour "k" 0]
                      (map fromFormatToIx input))
           `shouldBe` Just expectedSpec

  where
    expectedSpec = Specification . Left $ expectation
    fromFormatToIx [ri,rj,rk] =
      [offsetToIx "i" ri, offsetToIx "j" rj, offsetToIx "k" rk]


variations3D =
  [ ( [ [-1,0,-1], [0,0,-1], [-1,0,0], [0,0,0] ]
    ,  Exact $ Spatial Linear (Sum [Product [Backward 1 1 True, Backward 1 3 True, Centered 0 2 True]])
    )
  , ( [ [1,1,0], [0,1,0] ]
    ,  Exact $ Spatial Linear (Sum [Product [Forward 1 1 True, Forward 1 2 False, Centered 0 3 True]])
    )
  , ( [ [-1,0,-1], [0,0,-1], [-1,0,0], [0,0,0] ]
    ,  Exact $ Spatial Linear (Sum [Product [Backward 1 1 True, Backward 1 3 True, Centered 0 2 True]])
    )
  ]

prop_extract_synth_inverse :: F.Name -> Int -> Bool
prop_extract_synth_inverse v o =
     ixToNeighbour' [v] (offsetToIx v o) == Neighbour v o

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
