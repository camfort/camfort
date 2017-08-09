{-# LANGUAGE FlexibleContexts #-}

module Camfort.Specification.Hoare.ParserSpec (spec) where

import           Data.Data                          (Data)
import           Data.Either                        (isLeft)
import           Data.Foldable                      (traverse_)

import           Data.Generics.Uniplate.Operations  (Biplate, transformBi)

import           Camfort.Specification.Hoare.Lexer
import           Camfort.Specification.Hoare.Parser
import           Camfort.Specification.Hoare.Syntax
import           Camfort.Specification.Hoare.Types
import           Camfort.Specification.Parser       (runParser)
import qualified Camfort.Specification.Parser       as Parser
import qualified Language.Fortran.AST               as F
import qualified Language.Fortran.Util.Position     as F

import           Test.Hspec                         hiding (Spec)
import qualified Test.Hspec                         as Test

spec :: Test.Spec
spec = describe "Hoare - Parser" $ do
  let (.->) = (,)

  it "lexes" $ do
    let runTest (input, output) =
          lexer input `shouldBe` Right output

        tests =
          [ "\"x\"" .-> [TExpr "x"]
          , "\"x + y - 347\"" .-> [TExpr "x + y - 347"]
          , "static_assert invariant(\"x\" = \"4)7\" )" .->
            [ TStaticAssert, TInvariant
            , TLParen, TExpr "x", TEquals, TExpr "4)7", TRParen]
          , "static_assert pre(t)" .->
            [ TStaticAssert, TPre, TLParen, TTrue, TRParen]
          ]

    traverse_ runTest tests

    lexer "static_assert post(\"missing close quote)" `shouldSatisfy` isLeft

  it "parses" $ do
    let runTest (input, output) =
          parse input `shouldSatisfy` (matches (Right output))

        fvar n = F.ExpValue () defSpan (F.ValVariable n)
        x = fvar "x"
        y = fvar "y"
        z = fvar "z"

        num n = F.ExpValue () defSpan (F.ValInteger (show n))

        bin o e1 e2 = F.ExpBinary () defSpan o e1 e2
        add = bin F.Addition
        sub = bin F.Subtraction

        xA3 = x `add` num 3
        ySz = y `sub` z
        yAz = y `add` z

        x .== y = PFCompare (PCEq x y)
        x .< y = PFCompare (PCLess x y)
        x .> y = PFCompare (PCGreater x y)
        x .>= y = PFCompare (PCGreaterEq x y)

        x *&& y = PFLogical (PLAnd x y)
        x *|| y = PFLogical (PLOr x y)
        x *-> y = PFLogical (PLImpl x y)

        tests =
          [ "! static_assert pre(\"x\" = \"y\")"
            .->
            Specification SpecPre (x .== y)

          , "! static_assert pre(t)"
            .->
            Specification SpecPre (PFLogical (PLLit True))

          , "! static_assert invariant(\"x + 3\" < \"y - z\" & \"x + 3\" > \"y + z\")"
            .->
            Specification SpecInvariant
            ((xA3 .< ySz) *&& (xA3 .> yAz))

          , "! static_assert post(\"x + 3\" < \"y - z\" & \"x + 3\" > \"y + z\" | \"x\" >= \"7\")"
            .->
            Specification SpecPost
            (((xA3 .< ySz) *&& (xA3 .> yAz)) *||
              (x .>= (num 7))
            )

          , "! static_assert seq(\"x + 3\" < \"y - z\" -> \"x + 3\" > \"y + z\" -> \"x\" >= \"7\")"
            .->
            Specification SpecSeq
            ((xA3 .< ySz) *->
              (((xA3 .> yAz)) *-> (x .>= (num 7)))
            )

          ]

    traverse_ runTest tests


stripSpans :: Data from => from -> from
stripSpans = transformBi (const defSpan)

-- | Check if the two inputs are equal after removing all source spans, which
-- this test suite doesn't care about.
matches :: (Eq a, Data a) => a -> a -> Bool
matches a b =
  stripSpans a == stripSpans b

defSpan = F.SrcSpan (F.Position 0 0 0) (F.Position 0 0 0)

parse :: String -> Either (Parser.SpecParseError HoareParseError) (PrimSpec ())
parse = runParser hoareParser
