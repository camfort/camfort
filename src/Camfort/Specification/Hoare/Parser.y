{

module Camfort.Specification.Hoare.Parser (hoareParser) where

import           Control.Monad.Except

import qualified Language.Fortran.AST as F

import           Language.Verification
import           Language.Expression.DSL hiding (Prop)

import qualified Camfort.Specification.Parser as Parser
import Camfort.Specification.Hoare.Syntax
import Camfort.Specification.Hoare.Lexer
import Camfort.Specification.Hoare.Parser.Types

}

%monad { HoareSpecParser } { >>= } { return }
%name parseHoare HOARE
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
  '<='          { TLE           }
  '>='          { TGE           }
  '<'           { TLT           }
  '>'           { TGT           }
  '='           { TEquals       }
  '&'           { TAnd          }
  '|'           { TOr           }
  '->'          { TImpl         }
  '<->'         { TEquiv        }
  '!'           { TNot          }
  '('           { TLParen       }
  ')'           { TRParen       }
  texpr         { TExpr $$      }


%left '|'
%nonassoc '<->'
%right '->'
%left '&'
%left '!'
%nonassoc '<=' '>=' '<' '>' '='


%%


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
| EXPR '=' EXPR         { PFCompare (PCEq $1 $3) }
-- | EXPR '!=' EXPR         { PFCompare (PCNeq $1 $3) }
| EXPR '<' EXPR         { PFCompare (PCLess $1 $3) }
| EXPR '>' EXPR         { PFCompare (PCGreater $1 $3) }
| EXPR '<=' EXPR        { PFCompare (PCLessEq $1 $3) }
| EXPR '>=' EXPR        { PFCompare (PCGreaterEq $1 $3) }

EXPR :: { F.Expression () }
: texpr {% parseExpression $1 }

{

parseError :: [Token] -> HoareSpecParser a
parseError = throwError . ParseError

hoareParser :: Parser.SpecParser HoareParseError (PrimSpec ())
hoareParser = Parser.mkParser (\src -> do
                                  tokens <- lexer src
                                  parseHoare tokens)
             ["static_assert", "invariant", "post", "pre", "seq"]

}
