{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.Parser.Types where

import           Control.Monad.Except
import           Data.Data
import           Control.Exception
import           Data.List               ( intercalate )

import qualified Data.ByteString.Char8    as B

import qualified Language.Fortran.AST     as F
import           Language.Fortran.Version ( FortranVersion(..) )

-- for temporary definition defined in new fortran-src version, copied here
import           Language.Fortran.AST ( A0, Statement )
import           Language.Fortran.Parser ( Parser, makeParserFixed, makeParserFree )
import qualified Language.Fortran.Parser.Fixed.Fortran66  as F66
import qualified Language.Fortran.Parser.Fixed.Fortran77  as F77
import qualified Language.Fortran.Parser.Free.Fortran90   as F90
import qualified Language.Fortran.Parser.Free.Fortran95   as F95
import qualified Language.Fortran.Parser.Free.Fortran2003 as F2003

data HoareParseError
  = UnmatchedQuote
  | UnexpectedInput
  | MalformedExpression String
  | MalformedTypeSpec String
  | ParseError [Token]
  | LexError String
  deriving (Eq, Ord, Typeable, Data)

instance Show HoareParseError where
  show UnmatchedQuote = "unmatched quote"
  show UnexpectedInput = "unexpected characters in input"
  show (MalformedExpression expr) = "couldn't parse expression: \"" ++ expr ++ "\""
  show (MalformedTypeSpec ts) = "couldn't parse type spec: \"" ++ ts ++ "\""
  show (ParseError tokens) = "unable to parse input: " ++ prettyTokens tokens
  show (LexError xs) = "unable to lex input: " ++ xs

instance Exception HoareParseError where

prettyTokens :: [Token] -> String
prettyTokens = intercalate " " . map prettyToken
  where
    prettyToken = \case
      TQuoted qv -> "\"" ++ qv ++ "\""
      TStaticAssert -> "static_assert"
      TPre -> "pre"
      TPost -> "post"
      TInvariant -> "invariant"
      TSeq -> "seq"
      TRParen -> ")"
      TLParen -> "("
      TAnd -> "&"
      TOr -> "|"
      TImpl -> "->"
      TEquiv -> "<->"
      TNot -> "!"
      TTrue -> "T"
      TFalse -> "F"
      TDeclAux -> "decl_aux"
      TName nm -> nm
      TDColon -> "::"

type HoareSpecParser = Either HoareParseError

data Token =

  -- Quoted Fortran --
    TQuoted String

  -- Static Assertions --
  | TStaticAssert
  | TPre
  | TPost
  | TInvariant
  | TSeq
  | TRParen
  | TLParen
  | TAnd
  | TOr
  | TImpl
  | TEquiv
  | TNot
  | TTrue
  | TFalse

  -- Auxiliary variable declarations --
  | TDeclAux
  | TName String
  | TDColon
  deriving (Show, Eq, Ord, Typeable, Data)

-- TODO: Make this report errors and deal with source position better
parseExpression :: FortranVersion -> String -> HoareSpecParser (F.Expression ())
parseExpression v strExpr =
  case byVerStmt v "<unknown>" paddedBsExpr of
    Right (F.StExpressionAssign _ _ _ e) -> return e
    _ -> throwError (MalformedExpression strExpr)
  where
    paddedBsExpr = B.pack $ "      a = " ++ strExpr


-- TODO: Make this report errors and deal with source position better
parseTypeSpec :: FortranVersion -> String -> HoareSpecParser (F.TypeSpec ())
parseTypeSpec v strTs =
  case byVerStmt v "<unknown>" paddedBsTs of
    Right (F.StDeclaration _ _ s _ _) -> return s
    _ -> throwError (MalformedTypeSpec strTs)
  where
    paddedBsTs = B.pack $ strTs ++ " :: dummy"

--------------------------------------------------------------------------------

f66StmtNoTransform, f77StmtNoTransform, f77eStmtNoTransform, f77lStmtNoTransform,
  f90StmtNoTransform, f95StmtNoTransform, f2003StmtNoTransform
    :: Parser (Statement A0)
f66StmtNoTransform   = makeParserFixed F66.statementParser   Fortran66
f77StmtNoTransform   = makeParserFixed F77.statementParser   Fortran77
f77eStmtNoTransform  = makeParserFixed F77.statementParser   Fortran77Extended
f77lStmtNoTransform  = makeParserFixed F77.statementParser   Fortran77Legacy
f90StmtNoTransform   = makeParserFree  F90.statementParser   Fortran90
f95StmtNoTransform   = makeParserFree  F95.statementParser   Fortran95
f2003StmtNoTransform = makeParserFree  F2003.statementParser Fortran2003

byVerStmt :: FortranVersion -> Parser (Statement A0)
byVerStmt = \case
  Fortran66         -> f66StmtNoTransform
  Fortran77         -> f77StmtNoTransform
  Fortran77Extended -> f77eStmtNoTransform
  Fortran77Legacy   -> f77lStmtNoTransform
  Fortran90         -> f90StmtNoTransform
  Fortran95         -> f95StmtNoTransform
  Fortran2003       -> f2003StmtNoTransform
  v                 -> error $  "Language.Fortran.Parser.byVerStmt: "
                             <> "no parser available for requested version: "
                             <> show v
