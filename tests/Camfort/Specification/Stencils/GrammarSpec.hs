module Camfort.Specification.Stencils.GrammarSpec (spec) where

import Data.Either (isLeft)

import Camfort.Specification.Stencils.Grammar
import Camfort.Specification.Stencils.Model (
    Approximation(..)
  , Multiplicity(..))

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

modifierTest :: String -> (Region -> Multiplicity (Approximation Region)) -> SpecWith ()
modifierTest modifiers f =
  it ("modified with " ++ modifiers) $ parse ("= stencil " ++ modifiers ++ " r1 + r2 :: a")
  `shouldBe` Right (SpecDec (Spec (f $ Or (Var "r1") (Var "r2"))) ["a"])

invalidStencilTest :: String -> String -> SpecWith ()
invalidStencilTest description stencilString =
  it description $ parse stencilString `shouldSatisfy` isLeft

spec :: Test.Spec
spec =
  describe "Stencils - Grammar" $ do
    it "basic unmodified stencil" $
      parse "= stencil r1 + r2 :: a"
      `shouldBe`
        Right (SpecDec (Spec . Mult . Exact $ Or (Var "r1") (Var "r2")) ["a"])

    context "with modifiers" $ do
      modifierTest "readOnce,"          (Once . Exact)
      modifierTest "atLeast,"           (Mult . (`Bound` Nothing) . Just)
      modifierTest "atMost,"            (Mult . Bound Nothing . Just)
      modifierTest "readOnce, atLeast," (Once . (`Bound` Nothing) . Just)
      modifierTest "readOnce, atMost,"  (Once . Bound Nothing . Just)

    describe "modifiers are case insensitive" $ do
      modifierTest "readOnce, atLeast," (Once . (`Bound` Nothing) . Just)
      modifierTest "readOnce, atMost,"  (Once . Bound Nothing . Just)
      modifierTest "readonce, atleast," (Once . (`Bound` Nothing) . Just)
      modifierTest "readonce, atmost,"  (Once . Bound Nothing . Just)

    describe "invalid stencils" $ do
      invalidStencilTest "approximation before multiplicity"
        "= stencil atLeast, readOnce r1 :: a"
      invalidStencilTest "repeated multiplicities"
        "= stencil readOnce, readOnce r1 :: a"
      invalidStencilTest "repeated approximations"
        "= stencil atLeast, atLeast, r1 :: a"
      invalidStencilTest "multiple approximations"
        "= stencil atLeast, atMost, r1 :: a"

{- Should no longer be possible
    it "just pointed stencil" $
      parse "= stencil pointed(dims=1,2) :: a"
      `shouldBe`
        Right (SpecDec (Spatial [Pointed [1, 2]] Nothing) ["a"])
-}


    it "basic modified stencil (1)" $
      parse "      = stencil readonce, r1 + r2 :: a"
      `shouldBe`
        Right (SpecDec (Spec . Once . Exact $ Or (Var "r1") (Var "r2")) ["a"])

{- Should no longer be possible
    it "basic monfieid stencil (2)" $
      parse "= stencil atleast, pointed(dims=1,2), \
             \       forward(depth=1, dim=1) :: x"
      `shouldBe`
        Right (SpecDec (Spatial [AtLeast,Pointed [1,2]] (Just $ Forward 1 1)) ["x"])

    it "basic stencil with pointed and nonpointed" $
      parse "= stencil atleast, pointed(dims=2),  \
            \        nonpointed(dims=1), forward(depth=1, dim=1) :: frob"
      `shouldBe`
        Right (SpecDec (Spatial [AtLeast, Nonpointed [1], Pointed [2]]
                               (Just $ Forward 1 1)) ["frob"])
-}

    it "region defn" $
      parse "= region :: r = forward(depth=1, dim=1) + backward(depth=2, dim=2)"
      `shouldBe`
        Right (RegionDec "r" (Or (Forward 1 1 True) (Backward 2 2 True)))

    it "region defn syntactic permutation" $
      parse "= region :: r = forward(dim=1,depth=1) + backward(depth=2, dim=2)"
      `shouldBe`
        Right (RegionDec "r" (Or (Forward 1 1 True) (Backward 2 2 True)))

    it "region defn irreflx syntactic permutation" $
      parse "= region :: r = forward(nonpointed,dim=1,depth=1) + backward(depth=2,nonpointed,dim=2)"
      `shouldBe`
        Right (RegionDec "r" (Or (Forward 1 1 False) (Backward 2 2 False)))

{- Should no longer be possible
    it "complex stencil" $
      parse "= stencil atleast, pointed(dims=1,2), readonce, \
            \ (forward(depth=1, dim=1) + r) * backward(depth=3, dim=4) \
            \ :: frob"
      `shouldBe`
       Right (SpecDec (Spatial [AtLeast,ReadOnce,Pointed [1,2]]
             (Just $ And (Or (Forward 1 1) (Var "r")) (Backward 3 4))) ["frob"])

    it "invalid stencil (atLeast/atMost)" $
      parse "= stencil atleast, atmost, pointed(dims=1,2), \
             \       forward(depth=1, dim=1) :: x"
      `shouldBe`
        (Left $ ProbablyAnnotation $
          "Conflicting modifiers: cannot use 'atLeast' and 'atMost' together")

    it "invalid stencil (pointed/nonpointed on same dim)" $
      parse "= stencil atleast, nonpointed(dims=2), pointed(dims=1,2), \
             \ forward(depth=1, dim=1) :: x"
      `shouldBe`
        (Left $ ProbablyAnnotation $
              "Conflicting modifiers: stencil marked as both\
              \ nonpointed and pointed in dimensions = 2")
-}


parse = specParser

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
