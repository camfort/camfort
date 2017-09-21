{-# LANGUAGE FlexibleContexts #-}

module Camfort.Specification.Hoare.ParserSpec (spec) where

import           Data.Data                                (Data)
import           Data.Either                              (isLeft)
import           Data.Foldable                            (traverse_)

import           Data.Generics.Uniplate.Operations        (Biplate, transformBi)

import           Camfort.Specification.Hoare.Lexer
import           Camfort.Specification.Hoare.Parser
import           Camfort.Specification.Hoare.Parser.Types
import           Camfort.Specification.Hoare.Syntax
import           Camfort.Specification.Parser             (runParser)
import qualified Camfort.Specification.Parser             as Parser
import qualified Language.Fortran.AST                     as F
import qualified Language.Fortran.Util.Position           as F

import           Test.Hspec                               hiding (Spec)
import qualified Test.Hspec                               as Test

spec :: Test.Spec
spec = describe "Hoare - Parser" $ do
  let (.->) = (,)

  it "lexes" $ do
    let runTest (input, output) =
          lexer input `shouldBe` Right output

        tests =
          [ "\"x\"" .-> [TQuoted "x"]
          , "\"x + y - 347\"" .-> [TQuoted "x + y - 347"]
          , "static_assert invariant(\"x == 4)7\" )" .->
            [ TStaticAssert, TInvariant
            , TLParen, TQuoted "x == 4)7", TRParen]
          , "static_assert pre(t)" .->
            [ TStaticAssert, TPre, TLParen, TTrue, TRParen]
          , "decl_aux(\"integer\" :: x)" .->
            [ TDeclAux, TLParen, TQuoted "integer", TDColon, TName "x", TRParen]
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

        (.==) = bin F.EQ
        (.<) = bin F.LT
        (.>) = bin F.GT
        (.>=) = bin F.GTE

        x *&& y = PFLogical (PLAnd x y)
        x *|| y = PFLogical (PLOr x y)
        x *-> y = PFLogical (PLImpl x y)

        tests =
          [ "! static_assert pre(\"x == y\")"
            .->
            SodSpec (Specification SpecPre (PFExpr $ x .== y))

          , "! static_assert pre(t)"
            .->
            SodSpec (Specification SpecPre (PFLogical (PLLit True)))

          , "! static_assert invariant(\"x + 3 < y - z\" & \"x + 3 > y + z\")"
            .->
            SodSpec (Specification SpecInvariant
                     ((PFExpr $ xA3 .< ySz) *&& (PFExpr $ xA3 .> yAz)))

          , "! static_assert post(\"x + 3 < y - z\" & \"x + 3 > y + z\" | \"x >= 7\")"
            .->
            SodSpec (Specification SpecPost
                     (((PFExpr $ xA3 .< ySz) *&& (PFExpr $ xA3 .> yAz)) *||
                      (PFExpr $ x .>= (num 7))
                     ))

          , "! static_assert seq(\"x + 3 < y - z\" -> \"x + 3 > y + z\" -> \"x >= 7\")"
            .->
            SodSpec (Specification SpecSeq
                     ((PFExpr $ xA3 .< ySz) *->
                      (((PFExpr $ xA3 .> yAz)) *-> (PFExpr $ x .>= (num 7)))
                     ))

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

parse :: String -> Either (Parser.SpecParseError HoareParseError) (SpecOrDecl ())
parse = runParser hoareParser
