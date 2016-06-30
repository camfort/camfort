module Camfort.Analysis.StencilSpecification.GrammarSpec (spec) where

import Camfort.Analysis.CommentAnnotator
import Camfort.Analysis.StencilSpecification.Grammar

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

spec :: Test.Spec
spec =
  describe "Stencils - Grammar" $ do
    it "basic unmodified stencil" $
      parse "!= stencil r1 + r2 :: a"
      `shouldBe`
        Right (SpecDec (Spatial [] (Or (Var "r1") (Var "r2"))) ["a"])

{- Should no longer be possible
    it "just reflexive stencil" $
      parse "!= stencil reflexive(dims=1,2) :: a"
      `shouldBe`
        Right (SpecDec (Spatial [Reflexive [1, 2]] Nothing) ["a"])
-}


    it "basic modified stencil (1)" $
      parse "      != stencil readonce, r1 + r2 :: a"
      `shouldBe`
        Right (SpecDec (Spatial [ReadOnce] (Or (Var "r1") (Var "r2"))) ["a"])

{- Should no longer be possible
    it "basic monfieid stencil (2)" $
      parse "!= stencil atleast, reflexive(dims=1,2), \
             \       forward(depth=1, dim=1) :: x"
      `shouldBe`
        Right (SpecDec (Spatial [AtLeast,Reflexive [1,2]] (Just $ Forward 1 1)) ["x"])

    it "basic stencil with reflexive and irreflexive" $
      parse "!= stencil atleast, reflexive(dims=2),  \
            \        irreflexive(dims=1), forward(depth=1, dim=1) :: frob"
      `shouldBe`
        Right (SpecDec (Spatial [AtLeast, Irreflexive [1], Reflexive [2]]
                               (Just $ Forward 1 1)) ["frob"])
-}

    it "region defn" $
      parse "!= region r = forward(depth=1, dim=1) + backward(depth=2, dim=2)"
      `shouldBe`
        Right (RegionDec "r" (Or (Forward 1 1) (Backward 2 2)))

    it "temporal" $
      parse "!= stencil dependency(a,b,c,foo), mutual :: foo, bar"
      `shouldBe`
       Right (SpecDec (Temporal ["a","b","c","foo"] True) ["foo", "bar"])

{- Should no longer be possible
    it "complex stencil" $
      parse "!= stencil atleast, reflexive(dims=1,2), readonce, \
            \ (forward(depth=1, dim=1) + r) * backward(depth=3, dim=4) \
            \ :: frob"
      `shouldBe`
       Right (SpecDec (Spatial [AtLeast,ReadOnce,Reflexive [1,2]]
             (Just $ And (Or (Forward 1 1) (Var "r")) (Backward 3 4))) ["frob"])

    it "invalid stencil (atLeast/atMost)" $
      parse "!= stencil atleast, atmost, reflexive(dims=1,2), \
             \       forward(depth=1, dim=1) :: x"
      `shouldBe`
        (Left $ ProbablyAnnotation $
          "Conflicting modifiers: cannot use 'atLeast' and 'atMost' together")

    it "invalid stencil (reflexive/irreflexive on same dim)" $
      parse "!= stencil atleast, irreflexive(dims=2), reflexive(dims=1,2), \
             \ forward(depth=1, dim=1) :: x"
      `shouldBe`
        (Left $ ProbablyAnnotation $
              "Conflicting modifiers: stencil marked as both\
              \ irreflexive and reflexive in dimensions = 2")
-}


parse = specParser

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
