module Camfort.Specification.Stencils.GrammarSpec (spec) where

import Data.Either (isLeft)

import Camfort.Specification.Stencils.Grammar
import Camfort.Specification.Stencils.Model (
    Approximation(..)
  , Multiplicity(..))
import qualified Camfort.Specification.Stencils.Syntax as Syn

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

    let dimDepthTest (depth, dim) =
          let depthDim = concat ["depth=", depth, ", dim=", dim]
          in it depthDim $
          parse (concat ["= stencil forward(", depthDim, ") :: a"])
          `shouldBe` Right (SpecDec (Spec . Mult . Exact $ RegionConst $
                                     Syn.Forward (read depth) (read dim) True) ["a"])

    describe "depth and dim" $
        mapM_ dimDepthTest [("1", "1"), ("10", "20")]

    describe "invalid stencils" $ do
      invalidStencilTest "approximation before multiplicity"
        "= stencil atLeast, readOnce r1 :: a"
      invalidStencilTest "repeated multiplicities"
        "= stencil readOnce, readOnce r1 :: a"
      invalidStencilTest "repeated approximations"
        "= stencil atLeast, atLeast, r1 :: a"
      invalidStencilTest "multiple approximations"
        "= stencil atLeast, atMost, r1 :: a"
      invalidStencilTest "zero dim"
        "= stencil forward(depth=1, dim=0) :: a"
      invalidStencilTest "zero depth"
        "= stencil forward(depth=0, dim=1) :: a"
      invalidStencilTest "negative dim"
        "= stencil forward(depth=1, dim=-1) :: a"
      invalidStencilTest "negative depth"
        "= stencil forward(depth=-1, dim=1) :: a"
      invalidStencilTest "just pointed stencil"
        "= stencil pointed(dims=1,2) :: a"
      invalidStencilTest "basic monfieid stencil (2)"
        "= stencil atleast, pointed(dims=1,2), \
             \       forward(depth=1, dim=1) :: x"
      invalidStencilTest "basic stencil with pointed and nonpointed"
        "= stencil atleast, pointed(dims=2),  \
            \        nonpointed(dims=1), forward(depth=1, dim=1) :: frob"
      invalidStencilTest "complex stencil"
        "= stencil atleast, pointed(dims=1,2), readonce, \
            \ (forward(depth=1, dim=1) + r) * backward(depth=3, dim=4) \
            \ :: frob"
      invalidStencilTest "pointed/nonpointed on same dim"
        "= stencil atleast, nonpointed(dims=2), pointed(dims=1,2), \
             \ forward(depth=1, dim=1) :: x"

    it "basic modified stencil (1)" $
      parse "      = stencil readonce, r1 + r2 :: a"
      `shouldBe`
        Right (SpecDec (Spec . Once . Exact $ Or (Var "r1") (Var "r2")) ["a"])


    let regionTestCase isRefl =
          Right (RegionDec "r"
                  (Or (RegionConst (Syn.Forward 1 1 isRefl))
                   (RegionConst (Syn.Backward 2 2 isRefl))))
    it "region defn" $
      parse "= region :: r = forward(depth=1, dim=1) + backward(depth=2, dim=2)"
      `shouldBe`
        regionTestCase True

    it "region defn syntactic permutation" $
      parse "= region :: r = forward(dim=1,depth=1) + backward(depth=2, dim=2)"
      `shouldBe`
        regionTestCase True

    it "region defn irreflx syntactic permutation" $
      parse "= region :: r = forward(nonpointed,dim=1,depth=1) + backward(depth=2,nonpointed,dim=2)"
      `shouldBe`
        regionTestCase False


parse = specParser

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
