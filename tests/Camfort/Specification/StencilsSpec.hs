{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Camfort.Specification.StencilsSpec (spec) where


import Control.Monad.Writer.Strict hiding (Sum, Product)
import Data.List

import Camfort.Helpers.Vec
import Camfort.Input
import Camfort.Specification.Stencils
import Camfort.Specification.Stencils.Generate
  (Neighbour(..), indicesToSpec, convIxToNeighbour)
import Camfort.Specification.Stencils.Synthesis
import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.InferenceBackend
import Camfort.Specification.Stencils.InferenceFrontend
import Camfort.Specification.Stencils.Syntax
import qualified Language.Fortran.AST as F
import Language.Fortran.Parser.Any (deduceVersion)
import Language.Fortran.ParserMonad
import Camfort.Reprint
import Camfort.Output

import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.Functor.Identity
import qualified Data.ByteString.Char8 as B

import System.Directory (listDirectory)
import System.FilePath

import Test.Hspec
import Test.QuickCheck

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
                ] :: [Vec (S (S (S Z))) Int])

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
        inferFromIndicesWithoutLinearity (VL fivepoint)
        `shouldBe`
         (Specification (Mult $ Exact $ Spatial
                     (Sum [ Product [ Centered 1 1 True, Centered 0 2 True]
                          , Product [ Centered 0 1 True, Centered 1 2 True]
                          ])) True)

      it "seven point stencil 2D" $
        inferFromIndicesWithoutLinearity (VL sevenpoint)
        `shouldBe`
          (Specification (Mult $ Exact $ Spatial
                       (Sum [ Product [ Centered 1 1 True, Centered 0 2 True, Centered 0 3 True]
                            , Product [ Centered 0 1 True, Centered 1 2 True, Centered 0 3 True]
                            , Product [ Centered 0 1 True, Centered 0 2 True, Centered 1 3 True]
                            ])) True)

      it "five point stencil 2D with blip" $
         inferFromIndicesWithoutLinearity (VL fivepointErr)
         `shouldBe`
          (Specification (Mult $ Exact $ Spatial
                         (Sum [ Product [ Centered 1 1 True, Centered 0 2 True],
                                Product [ Centered 0 1 True, Centered 1 2 True],
                                Product [ Forward 1 1 True, Forward 1 2 True] ])) True)

      it "centered forward" $
         inferFromIndicesWithoutLinearity (VL centeredFwd)
         `shouldBe`
          (Specification (Mult $ Exact $ Spatial
            (Sum [ Product [ Forward 1 1 True
                           , Centered 1 2 True] ])) True)

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
         `shouldBe` (Just $ Specification (Once $ Exact
                       (Spatial
                         (Sum [Product [Forward 1 1 False, Forward 1 2 False],
                               Product [Centered 0 1 True, Centered 0 2 True]]))) True)
      it "consistent (2) a(i,c,j) = b(i,j+1) + b(i,j) \
                        \:: forward(depth=1,dim=2)*pointed(dim=1)" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0, Constant (F.ValInteger "0"), Neighbour "j" 0]
                        [[offsetToIx "i" 0, offsetToIx "j" 1],
                         [offsetToIx "i" 0, offsetToIx "j" 0]]
         `shouldBe` (Just $ Specification (Once $ Exact
                       (Spatial
                         (Sum [Product [Centered 0 1 True, Forward 1 2 True]]))) True)

      it "consistent (3) a(i+1,c,j) = b(j,i+1) + b(j,i) \
                        \:: backward(depth=1,dim=2)*pointed(dim=1)" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 1, Constant (F.ValInteger "0"), Neighbour "j" 0]
                        [[offsetToIx "j" 0, offsetToIx "i" 1],
                         [offsetToIx "j" 0, offsetToIx "i" 0]]
         `shouldBe` (Just $ Specification (Once $ Exact
                       (Spatial
                         (Sum [Product [Centered 0 1 True, Backward 1 2 True]]))) True)

      it "consistent (4) a(i+1,j) = b(0,i+1) + b(0,i) \
                         \:: backward(depth=1,dim=2)" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 1, Neighbour "j" 0]
                        [[offsetToIx "j" absoluteRep, offsetToIx "i" 1],
                         [offsetToIx "j" absoluteRep, offsetToIx "i" 0]]
         `shouldBe` (Just $ Specification (Once $ Exact
                       (Spatial
                         (Sum [Product [Backward 1 2 True]]))) True)

      it "consistent (5) a(i) = b(i,i+1) \
                        \:: pointed(dim=1)*forward(depth=1,dim=2,nonpointed)" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0]
                        [[offsetToIx "i" 0, offsetToIx "i" 1]]
         `shouldBe` (Just $ Specification (Once $ Exact
                       (Spatial
                         (Sum [Product [Centered 0 1 True,
                                        Forward 1 2 False]]))) True)

      it "consistent (6) a(i) = b(i) + b(0) \
                        \:: pointed(dim=1)" $
        indicesToSpec' ["i", "j"]
                        [Neighbour "i" 0]
                        [[offsetToIx "i" 0], [offsetToIx "i" absoluteRep]]
         `shouldBe` Nothing

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

    let example2In = fixturesDir </> "example2.f"
    program <- runIO $ readParseSrcDir example2In []

    describe "integration test on inference for example2.f" $ do
      it "stencil infer" $
         fst (callAndSummarise (infer AssignMode '=') program)
           `shouldBe`
           "\ntests/fixtures/Specification/Stencils/example2.f\n\
            \(32:7)-(32:26)    stencil readOnce, (backward(depth=1, dim=1)) :: a\n\
            \(26:8)-(26:29)    stencil readOnce, (pointed(dim=1))*(pointed(dim=2)) :: a\n\
            \(24:8)-(24:53)    stencil readOnce, (pointed(dim=1))*(centered(depth=1, dim=2)) \
                                     \+ (centered(depth=1, dim=1))*(pointed(dim=2)) :: a\n\
            \(41:8)-(41:94)    stencil readOnce, (centered(depth=1, dim=2)) \
                                                \+ (centered(depth=1, dim=1)) :: a"

      it "stencil check" $
         fst (callAndSummarise (\f p -> (check f p, p)) program)
           `shouldBe`
           "\ntests/fixtures/Specification/Stencils/example2.f\n\
            \(23:1)-(23:82)    Correct.\n(31:1)-(31:56)    Correct."

    let example4In = fixturesDir </> "example4.f"
    program <- runIO $ readParseSrcDir example4In []

    describe "integration test on inference for example4.f" $
      it "stencil infer" $
         fst (callAndSummarise (infer AssignMode '=') program)
           `shouldBe`
            "\ntests/fixtures/Specification/Stencils/example4.f\n\
             \(6:8)-(6:33)    stencil readOnce, (pointed(dim=1)) :: x"

    describe "integration test on inference for example5" $
      describe "stencil synth" $ do
        assertStencilInferenceNoWarn "example5.f"
          "inserts correct comment types for old fortran"
        assertStencilInferenceNoWarn "example5.f90"
          "inserts correct comment types for modern fortran"

    describe "synth on files already containing stencils" $ do
      assertStencilInferenceNoWarn "example6.f"
        "complements existing stencils (when second missing)"
      assertStencilInferenceNoWarn "example7.f"
        "complements existing stencils (when none missing)"
      assertStencilInferenceNoWarn "example8.f"
        "complements existing stencils (when first missing)"
      assertStencilInferenceNoWarn "example9.f"
        "complements existing stencils (when none missing - only one stencil)"
      assertStencilInferenceNoWarn "example10.f"
        "complements existing stencils (when one missing - inside if)"
      assertStencilInferenceNoWarn "example13.f"
        "complements existing stencils (when using regions references)"
      assertStencilInferenceNoWarn "example11.f"
        "inserts correct access specification"
      assertStencilSynthResponse "example12.f"
        "reports errors when conflicting stencil exists"
        "\nEncountered the following errors when checking stencil specs for 'tests/fixtures/Specification/Stencils/example12.f'\n\n\
\(8:1)-(8:52)    Not well specified.\n\
\        Specification is:\n\
\                stencil readOnce, (backward(depth=1, dim=1)) :: a\n\
\\n\
\        but at (9:8)-(9:32) the code behaves as\n\
\                stencil readOnce, (forward(depth=1, dim=1)) :: a\n\n\
\Please resolve these errors, and then run synthesis again."
      assertStencilSynthResponseOut "example14.f"
        "warns when duplicate stencils exist, but continues"
        "\nEncountered the following errors when checking stencil specs for 'tests/fixtures/Specification/Stencils/example14.f'\n\n\
\(10:1)-(10:51)    Warning: Duplicate specification."
      assertStencilSynthResponseOut "example15.f"
        "warns when duplicate stencils exist (combined stencils), but continues"
        "\nEncountered the following errors when checking stencil specs for 'tests/fixtures/Specification/Stencils/example15.f'\n\n\
\(9:1)-(9:51)    Warning: Duplicate specification."

    sampleDirConts <- runIO $ listDirectory samplesDir
    expectedDirConts <- runIO $ listDirectory (samplesDir </> "expected")
    let hasExpectedSrcFile f = f `elem` expectedDirConts
        sampleFiles          = filter hasExpectedSrcFile sampleDirConts

    describe "sample file tests" $
        mapM_ (\file -> assertStencilInferenceSample
                file ("produces correct output file for " ++ file))
        sampleFiles

  where assertStencilInferenceDir expected dir fileName testComment =
          let file         = dir </> fileName
              version      = deduceVersion file
              expectedFile = expected dir fileName
          in do
            program          <- runIO $ readParseSrcDir file []
            programSrc       <- runIO $ readFile file
            synthExpectedSrc <- runIO $ readFile expectedFile
            it testComment $
              (B.unpack . runIdentity
               $ reprint (refactoring version)
               (snd . head . snd $ synth AssignMode '=' (map (\(f, _, p) -> (f, p)) program))
               (B.pack programSrc))
                `shouldBe` synthExpectedSrc
        assertStencilInferenceOnFile = assertStencilInferenceDir
          (\d f -> d </> getExpectedSrcFileName f) fixturesDir
        assertStencilInferenceSample = assertStencilInferenceDir
          (\d f -> d </> "expected" </> f) samplesDir
        assertStencilSynthResponse fileName testComment expectedResponse =
            let file = fixturesDir </> fileName
            in do
              program          <- runIO $ readParseSrcDir file []
              programSrc       <- runIO $ readFile file
              it testComment $ (fst $ synth AssignMode '=' (map (\(f, _, p) -> (f, p)) program))
                `shouldBe` expectedResponse
        assertStencilSynthResponseOut fileName testComment expectedResponse =
          describe testComment $ do
            assertStencilInferenceOnFile fileName "correct synthesis"
            assertStencilSynthResponse fileName "correct output" expectedResponse
        assertStencilInferenceNoWarn fileName testComment = assertStencilSynthResponseOut fileName testComment ""
        fixturesDir = "tests" </> "fixtures" </> "Specification" </> "Stencils"
        samplesDir  = "samples" </> "stencils"
        getExpectedSrcFileName file =
          let oldExtension = takeExtension file
          in addExtension (replaceExtension file "expected") oldExtension

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
instance {-# OVERLAPPING #-} Arbitrary a => Arbitrary (Vec Z a) where
    arbitrary = return Nil

instance (Arbitrary (Vec n a), Arbitrary a) => Arbitrary (Vec (S n) a) where
    arbitrary = do x  <- arbitrary
                   xs <- arbitrary
                   return $ Cons x xs

test2DSpecVariation a b (input, expectation) =
    it ("format=" ++ show input) $
       -- Test inference
       indicesToSpec' ["i", "j"] [a, b] (map fromFormatToIx input)
          `shouldBe` Just expectedSpec
  where
    expectedSpec = Specification expectation True
    fromFormatToIx [ri,rj] = [ offsetToIx "i" ri, offsetToIx "j" rj ]

indicesToSpec' ivs lhs = fst . runWriter . indicesToSpec ivmap "a" lhs
  where ivmap = IM.singleton 0 (S.fromList ivs)

variations =
  [ ( [ [0,0] ]
    , Once $ Exact $ Spatial (Sum [Product [ Centered 0 1 True, Centered 0 2 True]])
    )
  , ( [ [1,0] ]
    , Once $ Exact $ Spatial (Sum [Product [Forward 1 1 False, Centered 0 2 True]])
    )
  , ( [ [1,0], [0,0], [0,0] ]
    , Mult $ Exact $ Spatial (Sum [Product [Forward 1 1 True, Centered 0 2 True]])
    )
  , ( [ [0,1], [0,0] ]
    , Once $ Exact $ Spatial (Sum [Product [Centered 0 1 True, Forward 1 2 True]])
    )
  , ( [ [1,1], [0,1], [1,0], [0,0] ]
    , Once $ Exact $ Spatial (Sum [Product [Forward 1 1 True, Forward 1 2 True]])
    )
  , ( [ [-1,0], [0,0] ]
    , Once $ Exact $ Spatial (Sum [Product [Backward 1 1 True, Centered 0 2 True]])
    )
  , ( [ [0,-1], [0,0], [0,-1] ]
    , Mult $ Exact $ Spatial (Sum [Product [Centered 0 1 True, Backward 1 2 True]])
    )
  , ( [ [-1,-1], [0,-1], [-1,0], [0,0], [0, -1] ]
    , Mult $ Exact $ Spatial (Sum [Product [Backward 1 1 True, Backward 1 2 True]])
    )
  , ( [ [0,-1], [1,-1], [0,0], [1,0], [1,1], [0,1] ]
    , Once $ Exact $ Spatial $ Sum [ Product [ Forward 1 1 True, Centered 1 2 True] ]
    )
   -- Stencil which is non-contiguous in one direction
  , ( [ [0, 4], [1, 4] ]
    , Once $ Bound Nothing
                   (Just (Spatial (Sum [ Product [ Forward 1 1 True
                                                 , Forward 4 2 False ] ])))
    )
  ]

variationsRel =
  [   -- Stencil which has non-relative indices in one dimension
    (Neighbour "i" 0, Constant (F.ValInteger "0"), [ [0, absoluteRep], [1, absoluteRep] ]
    , Once $ Exact $ Spatial (Sum [Product [Forward 1 1 True]])
    )
  , (Neighbour "i" 1, Neighbour "j" 0, [ [0,0] ]
    , Once $ Exact $ Spatial (Sum [Product [ Backward 1 1 False, Centered 0 2 True]])
    )
  , (Neighbour "i" 0, Neighbour "j" 1, [ [0,1] ]
    , Once $ Exact $ Spatial (Sum [Product [Centered 0 1 True, Centered 0 2 True]])
    )
  , (Neighbour "i" 1, Neighbour "j" (-1), [ [1,0], [0,0], [0,0] ]
    , Mult $ Exact $ Spatial (Sum [Product [Backward 1 1 True, Forward 1 2 False]])
    )
  , (Neighbour "i" 0, Neighbour "j" (-1), [ [0,1], [0,0] ]
    , Once $ Exact $ Spatial (Sum [Product [Centered 0 1 True, Forward 2 2 False]])
    )
  -- [0,1] [0,0] [0,-1]
  , (Neighbour "i" 1, Neighbour "j" 0, [ [1,1], [1,0], [1,-1] ]
    , Once $ Exact $ Spatial (Sum [Product [Centered 0 1 True, Centered 1 2 True]])
    )
  , (Neighbour "i" 1, Neighbour "j" 0, [ [-2,0], [-1,0] ]
    , Once $ Bound Nothing
                   (Just (Spatial (Sum [Product [ Backward 3 1 False
                                                , Centered 0 2 True ]]))))

  , (Constant (F.ValInteger "0"), Neighbour "j" 0, [ [absoluteRep,1], [absoluteRep,0], [absoluteRep,-1] ]
    , Once $ Exact $ Spatial (Sum [Product [Centered 1 2 True]])
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
    expectedSpec = Specification expectation True
    fromFormatToIx [ri,rj,rk] =
      [offsetToIx "i" ri, offsetToIx "j" rj, offsetToIx "k" rk]


variations3D =
  [ ( [ [-1,0,-1], [0,0,-1], [-1,0,0], [0,0,0] ]
    ,  Once $ Exact $ Spatial (Sum [Product [Backward 1 1 True, Centered 0 2 True, Backward 1 3 True]])
    )
  , ( [ [1,1,0], [0,1,0] ]
    ,  Once $ Exact $ Spatial (Sum [Product [Forward 1 1 True, Forward 1 2 False, Centered 0 3 True]])
    )
  , ( [ [-1,0,-1], [0,0,-1], [-1,0,0], [0,0,0] ]
    ,  Once $ Exact $ Spatial (Sum [Product [Backward 1 1 True, Centered 0 2 True, Backward 1 3 True]])
    )
  ]

prop_extract_synth_inverse :: F.Name -> Int -> Bool
prop_extract_synth_inverse v o =
     convIxToNeighbour [v] (offsetToIx v o) == Neighbour v o

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
