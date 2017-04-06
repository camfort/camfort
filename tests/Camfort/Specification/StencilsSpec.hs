{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Camfort.Specification.StencilsSpec (spec) where

import GHC.TypeLits

import Control.Monad.Writer.Strict hiding (Sum, Product)
import Data.List

import Camfort.Functionality
import Camfort.Helpers.Vec
import Camfort.Input
import Camfort.Specification.Stencils
import Camfort.Specification.Stencils.Synthesis
import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.InferenceBackend
import Camfort.Specification.Stencils.InferenceFrontend
import Camfort.Specification.Stencils.Syntax hiding (Spec)
import Camfort.Analysis.Annotations
import qualified Language.Fortran.AST as F
import Language.Fortran.Util.Position
import Language.Fortran.ParserMonad
import Camfort.Reprint
import Camfort.Output

import Data.Map.Strict (toList)
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.Functor.Identity
import qualified Data.ByteString.Char8 as B

import System.FilePath

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "Stencils" $ do
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
                ] :: [Vec 3 Int])

    it "composeRegions (1,0)-(1,0) span and (2,0)-(2,0) span" $
      shouldBe (coalesce
                  (Cons 1 (Cons 0 Nil), Cons 1 (Cons 0 Nil))
                  (Cons 2 (Cons 0 Nil), Cons 2 (Cons 0 Nil)))
               $ Just (Cons 1 (Cons 0 Nil), Cons 2 (Cons 0 Nil))

    it "composeRegions failing on (1,0)-(2,0) span and (4,0)-(5,0) span" $
      shouldBe (coalesce
                  (Cons 1 (Cons 0 Nil), Cons 2 (Cons 0 Nil))
                  (Cons 4 (Cons 0 Nil), Cons 5 (Cons 0 Nil)))
               Nothing

    it "composeRegions failing on (1,0)-(2,0) span and (3,1)-(3,1) span" $
      shouldBe (coalesce
                  (Cons 1 (Cons 0 Nil), Cons 2 (Cons 0 Nil))
                  (Cons 3 (Cons 1 Nil), Cons 3 (Cons 1 Nil)))
               Nothing

    it "five point stencil 2D" $
      -- Sort the expected value for the sake of easy equality
      shouldBe (sort $ inferMinimalVectorRegions fivepoint)
               (sort [ (Cons (-1) (Cons 0 Nil), Cons 1 (Cons 0 Nil))
                     , (Cons 0 (Cons (-1) Nil), Cons 0 (Cons 1 Nil)) ])

    it "seven point stencil 3D" $
      shouldBe
        (sort $ inferMinimalVectorRegions sevenpoint)
        (sort
           [ (Cons (-1) (Cons 0 (Cons 0 Nil)), Cons 1 (Cons 0 (Cons 0 Nil)))
           , (Cons 0 (Cons (-1) (Cons 0 Nil)), Cons 0 (Cons 1 (Cons 0 Nil)))
           , (Cons 0 (Cons 0 (Cons (-1) Nil)), Cons 0 (Cons 0 (Cons 1 Nil))) ])

    describe "Example stencil inferences" $ do
      it "five point stencil 2D" $
        inferFromIndices (VL fivepoint)
        `shouldBe`
         (Specification $ Single $ Exact $ Spatial
                     (Sum [ Product [ Centered 0 1 True, Centered 1 2 True]
                          , Product [ Centered 0 2 True, Centered 1 1 True]
                          ]))

      it "seven point stencil 2D" $
        inferFromIndices (VL sevenpoint)
        `shouldBe`
          (Specification $ Single $ Exact $ Spatial
                       (Sum [ Product [ Centered 0 1 True, Centered 0 2 True, Centered 1 3 True]
                            , Product [ Centered 0 1 True, Centered 0 3 True, Centered 1 2 True]
                            , Product [ Centered 0 2 True, Centered 0 3 True, Centered 1 1 True]
                            ]))

      it "five point stencil 2D with blip" $
         inferFromIndices (VL fivepointErr)
         `shouldBe`
          (Specification $ Single $ Exact $ Spatial
                         (Sum [ Product [ Forward 1 1 True, Forward 1 2 True],
                                Product [ Centered 0 1 True, Centered 1 2 True],
                                Product [ Centered 0 2 True, Centered 1 1 True] ]))

      it "centered forward" $
         inferFromIndices (VL centeredFwd)
         `shouldBe`
          (Specification $ Single $ Exact $ Spatial
            (Sum [ Product [ Forward 1 1 True
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

    describe "Inconsistent induction variable usage tests" $ do
      it "consistent (1) a(i,j) = b(i+1,j+1) + b(i,j)" $
        indicesToSpec' ["i", "j"]
                       [Neighbour "i" 0, Neighbour "j" 0]
                       [[offsetToIx "i" 1, offsetToIx "j" 1],
                        [offsetToIx "i" 0, offsetToIx "j" 0]]
         `shouldBe` (Just $ Specification $ Single $ Exact
                       (Spatial
                         (Sum [Product [Forward 1 1 False, Forward 1 2 False],
                               Product [Centered 0 1 True, Centered 0 2 True]])))
      it "consistent (2) a(i,c,j) = b(i,j+1) + b(i,j) \
                        \:: forward(depth=1,dim=2)*pointed(dim=1)" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0, Constant (F.ValInteger "0"), Neighbour "j" 0]
                        [[offsetToIx "i" 0, offsetToIx "j" 1],
                         [offsetToIx "i" 0, offsetToIx "j" 0]]
         `shouldBe` (Just $ Specification $ Single $ Exact
                       (Spatial
                         (Sum [Product [Forward 1 2 True, Centered 0 1 True]])))

      it "consistent (3) a(i+1,c,j) = b(j,i+1) + b(j,i) \
                        \:: backward(depth=1,dim=2)*pointed(dim=1)" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 1, Constant (F.ValInteger "0"), Neighbour "j" 0]
                        [[offsetToIx "j" 0, offsetToIx "i" 1],
                         [offsetToIx "j" 0, offsetToIx "i" 0]]
         `shouldBe` (Just $ Specification $ Single $ Exact
                       (Spatial
                         (Sum [Product [Backward 1 2 True, Centered 0 1 True]])))

      it "consistent (4) a(i+1,j) = b(0,i+1) + b(0,i) \
                         \:: backward(depth=1,dim=2)" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 1, Neighbour "j" 0]
                        [[offsetToIx "j" absoluteRep, offsetToIx "i" 1],
                         [offsetToIx "j" absoluteRep, offsetToIx "i" 0]]
         `shouldBe` (Just $ Specification $ Single $ Exact
                       (Spatial
                         (Sum [Product [Backward 1 2 True]])))

      it "consistent (5) a(i) = b(i,i+1) \
                        \:: pointed(dim=1)*forward(depth=1,dim=2,nonpointed)" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0]
                        [[offsetToIx "i" 0, offsetToIx "i" 1]]
         `shouldBe` (Just $ Specification $ Single $ Exact
                       (Spatial
                         (Sum [Product [Forward 1 2 False,
                                        Centered 0 1 True]])))

      it "consistent (6) a(i) = b(i) + b(0) \
                        \:: pointed(dim=1)" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0]
                        [[offsetToIx "i" 0], [offsetToIx "i" absoluteRep]]
         `shouldBe` (Just $ Specification $ Single $ Exact
                       (Spatial
                         (Sum [Product [Centered 0 1 True]])))

      it "inconsistent (1) RHS" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0, Neighbour "j" 0]
                        [[offsetToIx "i" 1, offsetToIx "j" 1],
                         [offsetToIx "j" 0, offsetToIx "i" 0]]
         `shouldBe` Nothing

      it "inconsistent (2) RHS to LHS" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0]
                        [[offsetToIx "i" 1, offsetToIx "j" 1],
                         [offsetToIx "j" 0, offsetToIx "i" 0]]
         `shouldBe` Nothing

    -------------------------
    -- Some integration tests
    -------------------------

    let file = "tests"
           </> "fixtures"
           </> "Specification"
           </> "Stencils"
           </> "example2.f"
    let fileSynthExpected = "tests"
           </> "fixtures"
           </> "Specification"
           </> "Stencils"
           </> "example2.expected.f"
    program <- runIO $ readParseSrcDir file []
    programSrc       <- runIO $ readFile file
    synthExpectedSrc <- runIO $ readFile fileSynthExpected

    describe "integration test on inference for example2.f" $ do
      it "stencil infer" $
         fst (callAndSummarise (infer AssignMode '=') program)
           `shouldBe`
           "\ntests/fixtures/Specification/Stencils/example2.f\n\
            \(24:8)-(24:53)    stencil readOnce, (pointed(dim=1))*(centered(depth=1, dim=2)) \
                                     \+ (pointed(dim=2))*(centered(depth=1, dim=1)) :: a\n\
            \(32:7)-(32:26)    stencil readOnce, (backward(depth=1, dim=1)) :: a\n\
            \(41:8)-(41:103)    stencil readOnce, (centered(depth=1, dim=1)) \
                                                \+ (centered(depth=1, dim=2)) :: a\n\
            \(42:8)-(42:37)    stencil readOnce, (pointed(dim=1))*(pointed(dim=2)) :: a"

      it "stencil check" $
         fst (callAndSummarise (\f p -> (check f p, p)) program)
           `shouldBe`
           "\ntests/fixtures/Specification/Stencils/example2.f\n\
            \(23:1)-(23:82)    Correct.\n(31:1)-(31:56)    Correct."

      it "stencil synth" $
         (B.unpack . runIdentity
           $ reprint (refactoring Fortran77)
              (snd . head . snd $ synth AssignMode '=' (map (\(f, _, p) -> (f, p)) program))
              (B.pack programSrc))
          `shouldBe` synthExpectedSrc

    let file = "tests"
           </> "fixtures"
           </> "Specification"
           </> "Stencils"
           </> "example3.f"
    program <- runIO $ readParseSrcDir file []

    let file = "tests"
           </> "fixtures"
           </> "Specification"
           </> "Stencils"
           </> "example4.f"
    program <- runIO $ readParseSrcDir file []

    describe "integration test on inference for example4.f" $
      it "stencil infer" $
         fst (callAndSummarise (infer AssignMode '=') program)
           `shouldBe`
            "\ntests/fixtures/Specification/Stencils/example4.f\n\
             \(6:8)-(6:33)    stencil readOnce, (pointed(dim=1)) :: x"

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
              ] :: [ Vec 2 Int ]

-- Examples of unusal patterns
fivepointErr = [ Cons (-1) (Cons 0 Nil)
               , Cons 0 (Cons (-1) Nil)
               , Cons 1 (Cons 0 Nil)
               , Cons 0 (Cons 1 Nil)
               , Cons 0 (Cons 0 Nil)
               , Cons 1 (Cons 1 Nil) ] :: [ Vec 2 Int ]

{- Construct arbtirary vectors and test up to certain sizes -}
instance {-# OVERLAPPING #-} Arbitrary a => Arbitrary (Vec 0 a) where
    arbitrary = return Nil

instance (Arbitrary (Vec n a), Arbitrary a, n' ~ (n+1)) => Arbitrary (Vec n' a) where
    arbitrary = do x  <- arbitrary
                   xs <- arbitrary
                   return $ Cons x xs

test2DSpecVariation a b (input, expectation) =
    it ("format=" ++ show input) $
       -- Test inference
       indicesToSpec' ["i", "j"] [a, b] (map fromFormatToIx input)
          `shouldBe` Just expectedSpec
  where
    expectedSpec = Specification expectation
    fromFormatToIx [ri,rj] = [ offsetToIx "i" ri, offsetToIx "j" rj ]

indicesToSpec' ivs lhs = fst . runWriter . indicesToSpec ivmap "a" lhs
  where ivmap = IM.singleton 0 (S.fromList ivs)

variations =
  [ ( [ [0,0] ]
    , Single $ Exact $ Spatial (Sum [Product [ Centered 0 1 True, Centered 0 2 True]])
    )
  , ( [ [1,0] ]
    , Single $ Exact $ Spatial (Sum [Product [Forward 1 1 False, Centered 0 2 True]])
    )
  , ( [ [1,0], [0,0], [0,0] ]
    , Multiple $ Exact $ Spatial (Sum [Product [Forward 1 1 True, Centered 0 2 True]])
    )
  , ( [ [0,1], [0,0] ]
    , Single $ Exact $ Spatial (Sum [Product [Forward 1 2 True, Centered 0 1 True]])
    )
  , ( [ [1,1], [0,1], [1,0], [0,0] ]
    , Single $ Exact $ Spatial (Sum [Product [Forward 1 1 True, Forward 1 2 True]])
    )
  , ( [ [-1,0], [0,0] ]
    , Single $ Exact $ Spatial (Sum [Product [Backward 1 1 True, Centered 0 2 True]])
    )
  , ( [ [0,-1], [0,0], [0,-1] ]
    , Multiple $ Exact $ Spatial (Sum [Product [Backward 1 2 True, Centered 0 1 True]])
    )
  , ( [ [-1,-1], [0,-1], [-1,0], [0,0], [0, -1] ]
    , Multiple $ Exact $ Spatial (Sum [Product [Backward 1 1 True, Backward 1 2 True]])
    )
  , ( [ [0,-1], [1,-1], [0,0], [1,0], [1,1], [0,1] ]
    , Single $ Exact $ Spatial $ Sum [ Product [ Forward 1 1 True, Centered 1 2 True] ]
    )
   -- Stencil which is non-contiguous in one direction
  , ( [ [0, 4], [1, 4] ]
    , Single $ Bound (Just (Spatial (Sum [ Product [ Forward 1 1 True ] ])))
                     (Just (Spatial (Sum [ Product [ Forward 1 1 True
                                                   , Forward 4 2 True ] ])))
    )
  ]

variationsRel =
  [   -- Stencil which has non-relative indices in one dimension
    (Neighbour "i" 0, Constant (F.ValInteger "0"), [ [0, absoluteRep], [1, absoluteRep] ]
    , Single $ Exact $ Spatial (Sum [Product [Forward 1 1 True]])
    )
  , (Neighbour "i" 1, Neighbour "j" 0, [ [0,0] ]
    , Single $ Exact $ Spatial (Sum [Product [ Backward 1 1 False, Centered 0 2 True]])
    )
  , (Neighbour "i" 0, Neighbour "j" 1, [ [0,1] ]
    , Single $ Exact $ Spatial (Sum [Product [Centered 0 1 True, Centered 0 2 True]])
    )
  , (Neighbour "i" 1, Neighbour "j" (-1), [ [1,0], [0,0], [0,0] ]
    , Multiple $ Exact $ Spatial (Sum [Product [Forward 1 2 False, Backward 1 1 True]])
    )
  , (Neighbour "i" 0, Neighbour "j" (-1), [ [0,1], [0,0] ]
    , Single $ Exact $ Spatial (Sum [Product [Forward 2 2 False, Centered 0 1 True]])
    )
  -- [0,1] [0,0] [0,-1]
  , (Neighbour "i" 1, Neighbour "j" 0, [ [1,1], [1,0], [1,-1] ]
    , Single $ Exact $ Spatial (Sum [Product [Centered 0 1 True, Centered 1 2 True]])
    )
  , (Neighbour "i" 1, Neighbour "j" 0, [ [-2,0], [-1,0] ]
    , Single $ Bound (Just (Spatial (Sum [Product [ Centered 0 2 True ]])))
                     (Just (Spatial (Sum [Product [ Backward 3 1 True
                                                  , Centered 0 2 True ]]))))

  , (Constant (F.ValInteger "0"), Neighbour "j" 0, [ [absoluteRep,1], [absoluteRep,0], [absoluteRep,-1] ]
    , Single $ Exact $ Spatial (Sum [Product [Centered 1 2 True]])
    )
  ]

test3DSpecVariation (input, expectation) =
    it ("format=" ++ show input) $
      -- Test inference
      indicesToSpec' ["i", "j", "k"]
                     [Neighbour "i" 0, Neighbour "j" 0, Neighbour "k" 0]
                     (map fromFormatToIx input)
           `shouldBe` Just expectedSpec

  where
    expectedSpec = Specification expectation
    fromFormatToIx [ri,rj,rk] =
      [offsetToIx "i" ri, offsetToIx "j" rj, offsetToIx "k" rk]


variations3D =
  [ ( [ [-1,0,-1], [0,0,-1], [-1,0,0], [0,0,0] ]
    ,  Single $ Exact $ Spatial (Sum [Product [Backward 1 1 True, Backward 1 3 True, Centered 0 2 True]])
    )
  , ( [ [1,1,0], [0,1,0] ]
    ,  Single $ Exact $ Spatial (Sum [Product [Forward 1 1 True, Forward 1 2 False, Centered 0 3 True]])
    )
  , ( [ [-1,0,-1], [0,0,-1], [-1,0,0], [0,0,0] ]
    ,  Single $ Exact $ Spatial (Sum [Product [Backward 1 1 True, Backward 1 3 True, Centered 0 2 True]])
    )
  ]

prop_extract_synth_inverse :: F.Name -> Int -> Bool
prop_extract_synth_inverse v o =
     ixToNeighbour' [v] (offsetToIx v o) == Neighbour v o

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
