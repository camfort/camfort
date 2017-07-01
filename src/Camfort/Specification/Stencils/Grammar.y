{ -- -*- Mode: Haskell -*-
{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
module Camfort.Specification.Stencils.Grammar
( specParser, Specification(..), Region(..), SpecInner(..), lexer ) where

import Data.Char (isLetter, isNumber, isAlphaNum, toLower, isAlpha, isSpace)
import Data.List (intersect, sort, isPrefixOf, isInfixOf, intercalate)
import Data.Data
import qualified Data.Text as T

import Debug.Trace

import Camfort.Analysis.CommentAnnotator
import Camfort.Specification.Stencils.Model (Approximation(..), Multiplicity(..))
import qualified Camfort.Specification.Stencils.Syntax as Syn

}

%monad { Either AnnotationParseError } { >>= } { return }
%name parseSpec SPEC
%tokentype { Token }
%token
  stencil     { TId _ "stencil" }
  access      { TId _ "access" }
  region      { TId _ "region" }
  readOnce    { TId _ "readonce" }
  pointed     { TId _ "pointed" }
  nonpointed  { TId _ "nonpointed" }
  atMost      { TId _ "atmost" }
  atLeast     { TId _ "atleast" }
  dim         { TId _ "dim" }
  depth       { TId _ "depth" }
  forward     { TId _ "forward" }
  backward    { TId _ "backward" }
  centered    { TId _ "centered" }
  id          { TId _ $$ }
  num         { TNum $$ }
  '+'         { TPlus }
  '*'         { TStar }
  '::'        { TDoubleColon }
  '='         { TEqual }
  '('         { TLParen }
  ')'         { TRParen }

%left '+'
%left '*'

%%

SPEC :: { Specification }
: REGIONDEC                 { RegionDec (fst $1) (snd $1) }
| stencil SPECDEC '::' VARS { SpecDec ($2 True) $4 }
| access  SPECDEC '::' VARS { SpecDec ($2 False) $4 }

REGIONDEC :: { (String, Region) }
: region '::' id '=' REGION { ($3, $5) }

REGION ::                       { Region }
: REGIONCONST                   { RegionConst $1 }
| REGION '+' REGION             { Or $1 $3 }
| REGION '*' REGION             { And $1 $3 }
| '(' REGION ')'                { $2 }
| id                            { Var $1 }

REGIONCONST :: { Syn.Region }
: forward  '(' REGION_ATTRS ')' { applyAttr Syn.Forward  $3 }
| backward '(' REGION_ATTRS ')' { applyAttr Syn.Backward $3 }
| centered '(' REGION_ATTRS ')' { applyAttr Syn.Centered $3 }
| pointed  '(' dim '=' num ')'  { Syn.Centered 0 (read $5) True }

REGION_ATTRS :: { (Depth Int, Dim Int, Bool) }
  : DEPTH DIM_REFL    { ($1, fst $2, snd $2) }
  | DIM   DEPTH_REFL  { (fst $2, $1, snd $2) }
  | REFL  DEPTH DIM   { ($2, $3, $1) }
  | REFL  DIM DEPTH   { ($3, $2, $1) }

DIM_REFL :: { (Dim Int, Bool) }
DIM_REFL
   : REFL DIM { ($2, $1) }
   | DIM REFL { ($1, $2) }
   | DIM      { ($1, True) }

DEPTH_REFL :: { (Depth Int, Bool) }
DEPTH_REFL
   : DEPTH REFL { ($1, $2) }
   | REFL DEPTH { ($2, $1) }
   | DEPTH      { ($1, True) }

DEPTH :: { Depth Int }
DEPTH : depth '=' num { Depth $ read $3 }

DIM :: { Dim Int }
DIM : dim '=' num { Dim $ read $3 }

REFL :: { Bool }
 : nonpointed { False }

SPECDEC :: { Syn.IsStencil -> SpecInner }
: MULTIPLICITY { SpecInner $1 }

MULTIPLICITY ::          { Multiplicity (Approximation Region) }
: readOnce APPROXIMATION { Once $2 }
| APPROXIMATION          { Mult $1 }

APPROXIMATION :: { Approximation Region }
: atLeast REGION { Bound (Just $2) Nothing }
| atMost REGION  { Bound Nothing (Just $2) }
| REGION         { Exact $1 }

VARS :: { [String] }
: id VARS { $1 : $2 }
| id      { [$1] }

{
newtype Depth a = Depth a
newtype Dim a = Dim a

applyAttr :: (Int -> Int -> Bool -> Syn.Region)
          -> (Depth Int, Dim Int, Bool)
          -> Syn.Region
applyAttr constr (Depth d, Dim dim, irrefl) = constr d dim irrefl

data Specification
  = RegionDec String Region
  | SpecDec SpecInner [String]
  deriving (Show, Eq, Typeable, Data)

data Region
  = RegionConst Syn.Region
  | Or Region Region
  | And Region Region
  | Var String
  deriving (Show, Eq, Ord, Typeable, Data)

data SpecInner = SpecInner
    (Multiplicity (Approximation Region))  -- main specification content
    Syn.IsStencil                          -- a bool: stencil or access
  deriving (Show, Eq, Typeable, Data)

--------------------------------------------------

data Token
  = TDoubleColon
  | TStar
  | TPlus
  | TEqual
  | TComma
  | TLParen
  | TRParen
  | TId String String -- first string contains the original text
                      -- second is normalised (e.g., for keywords)
  | TNum String
 deriving (Show, Eq)

addToTokens :: Token -> String -> Either AnnotationParseError [ Token ]
addToTokens tok rest = do
 tokens <- lexer' rest
 return $ tok : tokens

lexer :: String -> Either AnnotationParseError [ Token ]
lexer input | length (stripLeadingWhiteSpace input) >= 2 =
  case stripLeadingWhiteSpace input of
    -- Check the leading character is '=' for specification
    '=':input' -> testAnnotation input'
    '!':input' -> testAnnotation input'
    '>':input' -> testAnnotation input'
    '<':input' -> testAnnotation input'
    _ -> Left NotAnnotation
  where
    stripLeadingWhiteSpace = T.unpack . T.strip . T.pack
    testAnnotation inp =
      -- First test to see if the input looks like an actual
      -- specification of either a stencil or region
      if (inp `hasPrefix` "stencil" || inp `hasPrefix` "region"
                                    || inp `hasPrefix` "access")
      then lexer' inp
      else Left NotAnnotation
    hasPrefix []       str = False
    hasPrefix (' ':xs) str = hasPrefix xs str
    hasPrefix xs       str = isPrefixOf str xs
lexer _ = Left NotAnnotation


lexer' :: String -> Either AnnotationParseError [ Token ]
lexer' []                                              = return []
lexer' (' ':xs)                                        = lexer' xs
lexer' ('\t':xs)                                       = lexer' xs
lexer' (':':':':xs)                                    = addToTokens TDoubleColon xs
lexer' ('*':xs)                                        = addToTokens TStar xs
lexer' ('+':xs)                                        = addToTokens TPlus xs
lexer' ('=':xs)                                        = addToTokens TEqual xs
-- Comma hack: drop commas that are not separating numbers,
-- in order to avoid need for 2-token lookahead.
lexer' (',':xs)
  | x':xs' <- dropWhile isSpace xs, not (isNumber x') = lexer' (x':xs')
  | otherwise                                         = addToTokens TComma xs
lexer' ('(':xs)                                       = addToTokens TLParen xs
lexer' (')':xs)                                       = addToTokens TRParen xs
lexer' (x:xs)
  | isLetter x                                        =
        aux (\x -> TId x $ fmap toLower x) $ \ c -> isAlphaNum c || c == '_'
  | isPositiveNumber x                                = aux TNum isNumber
  | otherwise
     = failWith $ "Not an indentifier " ++ show x
 where
   isPositiveNumber x = isNumber x && x /= '0'
   aux f p = (f target :) <$> lexer' rest
     where (target, rest) = span p (x:xs)
lexer' x
    = failWith $ "Not a valid piece of stencil syntax " ++ show x

--------------------------------------------------

-- specParser :: String -> Either AnnotationParseError Specification
specParser :: AnnotationParser Specification
specParser src = do
 tokens <- lexer src
 parseSpec tokens

happyError :: [ Token ] -> Either AnnotationParseError a
happyError t =
  failWith $ "Could not parse specification at: \"" ++ prettyTokens t ++ "\""
            ++ reason t ++ "\n"

-- | Possible reasons for parse failure
reason :: [ Token ] -> String
reason (TId s "readonce" : _) =
    "\nPossible reason: '" ++ s ++ "' not placed at the start of the specification"
reason _ = ""

-- | Pretty-print the tokens, showing the smallest unique prefix of tokens
prettyTokens :: [ Token ] -> String
prettyTokens =
    (++ "... ") . intercalate " " . map prettyToken . takeUniquePrefix 1
  where
    takeUniquePrefix n ts =
      if ((take n ts) `isInfixOf` (drop n ts))
      then takeUniquePrefix (n+1) ts
      else take n ts

prettyToken TDoubleColon = "::"
prettyToken TStar        = "*"
prettyToken TPlus        = "+"
prettyToken TEqual       = "="
prettyToken TComma       = ","
prettyToken TLParen      = "("
prettyToken TRParen      = ")"
prettyToken (TId s _)    = s
prettyToken (TNum n)     = n

}
