{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.Types where

import           Control.Monad.Except
import           Data.List                         (intercalate)

import qualified Data.ByteString.Char8             as B
import qualified Language.Fortran.AST              as F
import qualified Language.Fortran.Lexer.FreeForm   as F
import qualified Language.Fortran.Parser.Fortran90 as F
import qualified Language.Fortran.ParserMonad      as F


data HoareParserError
  = UnmatchedQuote
  | UnexpectedInput
  | MalformedExpression String
  | ParseError [Token]

instance Show HoareParserError where
  show UnmatchedQuote = "unmatched quote"
  show UnexpectedInput = "unexpected characters in input"
  show (MalformedExpression expr) = "couldn't parse expression: \"" ++ expr ++ "\""
  show (ParseError tokens) = "unable to parse input: " ++ prettyTokens tokens

prettyTokens :: [Token] -> String
prettyTokens = intercalate " " . map prettyToken
  where
    prettyToken = \case
      TExpr expr -> "\"" ++ expr ++ "\""
      TStaticAssert -> "static_assert"
      TPre -> "pre"
      TPost -> "post"
      TInvariant -> "invariant"
      TSeq -> "seq"
      TEquals -> "="
      TGT -> ">"
      TLT -> "<"
      TGE -> ">="
      TLE -> "<="
      TRParen -> ")"
      TLParen -> "("
      TAnd -> "&"
      TOr -> "|"
      TImpl -> "->"
      TEquiv -> "<->"
      TNot -> "!"

type HoareSpecParser = Either HoareParserError

data Token
  = TExpr String
  | TStaticAssert
  | TPre
  | TPost
  | TInvariant
  | TSeq
  | TEquals
  | TGT
  | TLT
  | TGE
  | TLE
  | TRParen
  | TLParen
  | TAnd
  | TOr
  | TImpl
  | TEquiv
  | TNot
  deriving (Show)

parseError :: [Token] -> HoareSpecParser a
parseError = throwError . ParseError

-- TODO: Make this report errors and deal with source position better
parseExpression :: String -> HoareSpecParser (F.Expression ())
parseExpression expr =
  case F.runParse F.statementParser parseState of
    F.ParseOk (F.StExpressionAssign _ _ _ e) _ -> return e
    _ -> throwError (MalformedExpression expr)
  where
    paddedExpr = B.pack $ "      a = " ++ expr
    parseState = F.initParseState paddedExpr F.Fortran90 "<unknown>"
