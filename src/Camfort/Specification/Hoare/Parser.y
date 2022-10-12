{

module Camfort.Specification.Hoare.Parser (hoareParser) where

import           Control.Monad.Except

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Version as F

import           Language.Verification
import           Language.Expression.Prop

import qualified Camfort.Specification.Parser as Parser
import Camfort.Specification.Hoare.Syntax
import Camfort.Specification.Hoare.Lexer
import Camfort.Specification.Hoare.Parser.Types

}

%monad { HoareSpecParser } { >>= } { return }
%name parseHoare START
%tokentype { Token }
%error { parseError }
%token
  static_assert { TStaticAssert }
  invariant     { TInvariant    }
  post          { TPost         }
  pre           { TPre          }
  seq           { TSeq          }
  true          { TTrue         }
  false         { TFalse        }
  '&'           { TAnd          }
  '|'           { TOr           }
  '->'          { TImpl         }
  '<->'         { TEquiv        }
  '!'           { TNot          }
  '('           { TLParen       }
  ')'           { TRParen       }
  tquoted       { TQuoted $$    }
  decl_aux      { TDeclAux      }
  '::'          { TDColon       }
  tname         { TName $$      }


%left '|'
%nonassoc '<->'
%right '->'
%left '&'
%left '!'
%nonassoc '<=' '>=' '<' '>' '='


%%

START :: { SpecOrDecl () }
: HOARE { SodSpec $1 }
| DECL { SodDecl $1 }


DECL :: { AuxDecl () }
: decl_aux '(' TYPESPEC '::' tname ')' { AuxDecl $5 $3 }

TYPESPEC :: { F.TypeSpec () }
: tquoted {% parseTypeSpec F.Fortran90 $1 }


HOARE :: { PrimSpec () }
: static_assert SPEC { $2 }


SPEC :: { PrimSpec () }
: KIND '(' FORMULA ')' { Specification $1 $3 }


KIND :: { SpecKind }
: pre       { SpecPre }
| post      { SpecPost }
| seq       { SpecSeq }
| invariant { SpecInvariant }


FORMULA :: { PrimFormula () }
: true                  { PFLogical (PLLit True) }
| false                 { PFLogical (PLLit False) }
| FORMULA '&' FORMULA   { PFLogical (PLAnd $1 $3) }
| FORMULA '|' FORMULA   { PFLogical (PLOr $1 $3) }
| FORMULA '->' FORMULA  { PFLogical (PLImpl $1 $3) }
| FORMULA '<->' FORMULA { PFLogical (PLEquiv $1 $3) }
| '!' FORMULA           { PFLogical (PLNot $2) }
| '(' FORMULA ')'       { $2 }
| EXPR                  { PFExpr $1 }

EXPR :: { F.Expression () }
: tquoted {% parseExpression F.Fortran90 $1 }

{

parseError :: [Token] -> HoareSpecParser a
parseError = throwError . ParseError

hoareParser :: Parser.SpecParser HoareParseError (SpecOrDecl ())
hoareParser = Parser.mkParser (\src -> do
                                  tokens <- lexer src
                                  parseHoare tokens)
             ["static_assert", "invariant", "post", "pre", "seq", "decl_aux"]

}

{-
-*- mode: Haskell -*-
Local variables:
eval: (flycheck-mode nil)
End:
-}
