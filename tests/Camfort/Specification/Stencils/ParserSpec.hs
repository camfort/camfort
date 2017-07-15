module Camfort.Specification.Stencils.ParserSpec (spec) where

import Data.Either (isLeft)

import Camfort.Specification.Parser (runParser)
import qualified Camfort.Specification.Parser as Parser
import Camfort.Specification.Stencils.Parser
import Camfort.Specification.Stencils.Model (
    Approximation(..)
  , Multiplicity(..))
import qualified Camfort.Specification.Stencils.Syntax as Syn

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

-- | Helper for building stencil strings.
stencilString :: String -> String
stencilString body = "= stencil " ++ body ++ " :: a"

-- | Helper for building stencils.
mkSpec :: Multiplicity (Approximation Region) -> Either a Specification
mkSpec m = Right (SpecDec (SpecInner m True) ["a"])

modifierTest :: String -> (Region -> Multiplicity (Approximation Region)) -> SpecWith ()
modifierTest modifiers f =
  it ("modified with " ++ modifiers) $ parse (stencilString (modifiers ++ " r1 + r2"))
  `shouldBe` (mkSpec . f $ Or (Var "r1") (Var "r2"))

-- | Check that a stencil specification does not parse.
--
-- Automatically inserts a leading "= stencil " and trailing " :: a".
invalidStencilTest :: String -> String -> SpecWith ()
invalidStencilTest description stencilStr =
  it description $ parse (stencilString stencilStr) `shouldSatisfy` isLeft

-- | Check that a stencil specification does not parse.
--
-- Tests the @stencilStr@ as is.
invalidStencilTest' :: String -> String -> SpecWith ()
invalidStencilTest' description stencilStr =
  it description $ parse stencilStr `shouldSatisfy` isLeft

-- | Check that a stencil specification does not parse, and that the
-- error string matches that provided.
--
-- Tests the @stencilStr@ as is.
invalidStencilTestStr :: String -- ^ Test description
                      -> String -- ^ Specification string
                      -> String -- ^ Expected error string
                      -> SpecWith ()
invalidStencilTestStr description stencilStr errStr =
  it description $ show (parse stencilStr) `shouldBe` ("Left " ++ errStr)

spec :: Test.Spec
spec =
  describe "Stencils - Parser" $ do
    it "basic unmodified stencil" $
      parse (stencilString "r1 + r2")
      `shouldBe`
        mkSpec (Mult . Exact $ Or (Var "r1") (Var "r2"))

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
          parse (stencilString $ concat ["forward(", depthDim, ")"])
          `shouldBe` mkSpec (Mult . Exact $ RegionConst $
                                     Syn.Forward (read depth) (read dim) True)

    describe "depth and dim" $
        mapM_ dimDepthTest [("1", "1"), ("10", "20")]

    describe "invalid stencils" $ do
      invalidStencilTest "approximation before multiplicity"
        "atLeast, readOnce r1"
      invalidStencilTest "repeated multiplicities"
        "readOnce, readOnce r1"
      invalidStencilTest "repeated approximations"
        "atLeast, atLeast, r1"
      invalidStencilTest "multiple approximations"
        "atLeast, atMost, r1"
      invalidStencilTest "zero dim"
        "forward(depth=1, dim=0)"
      invalidStencilTest "zero depth"
        "forward(depth=0, dim=1)"
      invalidStencilTest "negative dim"
        "forward(depth=1, dim=-1)"
      invalidStencilTest "negative depth"
        "forward(depth=-1, dim=1)"
      invalidStencilTest "just pointed stencil"
        "pointed(dims=1,2)"
      invalidStencilTest "basic monfieid stencil (2)"
        "atleast, pointed(dims=1,2), forward(depth=1, dim=1)"
      invalidStencilTest "basic stencil with pointed and nonpointed"
        "atleast, pointed(dims=2),  \
            \        nonpointed(dims=1), forward(depth=1, dim=1)"
      invalidStencilTest "complex stencil"
        "atleast, pointed(dims=1,2), readonce, \
            \ (forward(depth=1, dim=1) + r) * backward(depth=3, dim=4)"
      invalidStencilTest "pointed/nonpointed on same dim"
        "atleast, nonpointed(dims=2), pointed(dims=1,2), \
             \ forward(depth=1, dim=1)"
      invalidStencilTest' "empty specification"
        "= stencil"
      invalidStencilTest' "only identifier"
        "= stencil foo"

    it "basic modified stencil (1)" $
      parse (stencilString "      readonce, r1 + r2")
      `shouldBe`
        mkSpec (Once . Exact $ Or (Var "r1") (Var "r2"))


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

    describe "error messages" $ do
      invalidStencilTestStr "invalid identifier"
        "= stencil foo$ :: a"
        "Invalid character in identifier: '$'"
      invalidStencilTestStr "invalid syntax"
        "= stencil readonce, readonce, pointed(dim=1) :: a"
        "Could not parse specification at: \"readonce... \"\n"

parse :: String -> Either (Parser.SpecParseError SpecParseError) Specification
parse = runParser specParser

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
