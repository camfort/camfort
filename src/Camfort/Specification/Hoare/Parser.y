{

module Camfort.Specification.Hoare.Parser (hoareParser) where

import           Control.Monad.Except

import qualified Language.Fortran.AST as F

import Language.While.Prop

import qualified Camfort.Specification.Parser as Parser
import Camfort.Specification.Hoare.Syntax
import Camfort.Specification.Hoare.Lexer
import Camfort.Specification.Hoare.Types

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


HOARE :: { Specification () }
: '=' static_assert SPEC { $3 }


SPEC :: { Specification () }
: KIND '(' FORMULA ')' { Specification $1 $3 }


KIND :: { SpecKind }
: pre       { SpecPre }
| post      { SpecPost }
| seq       { SpecSeq }
| invariant { SpecInvariant }


FORMULA :: { SpecFormula () }
: FORMULA '&' FORMULA   { $1 `PAnd` $3 }
| FORMULA '|' FORMULA   { $1 `POr` $3 }
| FORMULA '->' FORMULA  { $1 `PImpl` $3 }
| FORMULA '<->' FORMULA { $1 `PEquiv` $3 }
| '!' FORMULA           { PNot $2 }
| '(' FORMULA ')'       { $2 }
| EXPR '=' EXPR         { $1 `FEq` $3 }
| EXPR '<' EXPR         { $1 `FLT` $3 }
| EXPR '>' EXPR         { $1 `FGT` $3 }
| EXPR '<=' EXPR        { $1 `FLE` $3 }
| EXPR '>=' EXPR        { $1 `FGE` $3 }


EXPR :: { F.Expression () }
: texpr {% parseExpression $1 }

{

parseError :: [Token] -> HoareSpecParser a
parseError = throwError . ParseError

hoareParser :: Parser.SpecParser HoareParseError (Specification ())
hoareParser = Parser.mkParser (\src -> do
                                  tokens <- lexer src
                                  parseHoare tokens)
             ["static_assert", "invariant", "post", "pre", "seq"]

}
