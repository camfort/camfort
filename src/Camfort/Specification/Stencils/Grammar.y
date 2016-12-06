{ -- -*- Mode: Haskell -*-
{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
module Camfort.Specification.Stencils.Grammar
( specParser, Specification(..), Region(..), Spec(..), Mod(..), lexer ) where

import Data.Char (isLetter, isNumber, isAlphaNum, toLower, isAlpha, isSpace)
import Data.List (intersect, sort, isPrefixOf)
import Data.Data
import qualified Data.Text as T

import Debug.Trace

import Camfort.Analysis.CommentAnnotator
import Camfort.Specification.Stencils.Syntax (showL)

}

%monad { Either AnnotationParseError } { >>= } { return }
%name parseSpec SPEC
%tokentype { Token }
%token
  stencil     { TId "stencil" }
  region      { TId "region" }
  readOnce    { TId "readonce" }
  pointed     { TId "pointed" }
  nonpointed  { TId "nonpointed" }
  atMost      { TId "atmost" }
  atLeast     { TId "atleast" }
  dim         { TId "dim" }
  depth       { TId "depth" }
  forward     { TId "forward" }
  backward    { TId "backward" }
  centered    { TId "centered" }
  id          { TId $$ }
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
| stencil SPECDEC '::' VARS { SpecDec $2 $4 }

REGIONDEC :: { (String, Region) }
: region '::' id '=' REGION { ($3, $5) }

REGION ::                       { Region }
: forward  '(' REGION_ATTRS ')' { applyAttr Forward  $3 }
| backward '(' REGION_ATTRS ')' { applyAttr Backward $3 }
| centered '(' REGION_ATTRS ')' { applyAttr Centered $3 }
| pointed  '(' dim '=' num ')' { Centered 0 (read $5) True }
| REGION '+' REGION             { Or $1 $3 }
| REGION '*' REGION             { And $1 $3 }
| '(' REGION ')'                { $2 }
| id                            { Var $1 }

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

SPECDEC :: { Spec }
: APPROXMODS MOD REGION         { Spatial ($1 ++ [$2]) $3 }
| MOD REGION                    { Spatial [$1] $2 }
| APPROXMOD REGION               { Spatial [$1] $2 }
| REGION                         { Spatial [] $1 }

MOD :: { Mod }
: readOnce                          { ReadOnce }

-- Even though multiple approx mods is not allowed
-- allow them to be parsed so that the validator can
-- report a nice error if the user supplies more than one
APPROXMODS :: { [Mod] }
: APPROXMOD APPROXMODS { $1 : $2 }
| APPROXMOD            { [$1] }

APPROXMOD :: { Mod }
: atMost                    { AtMost }
| atLeast                   { AtLeast }

VARS :: { [String] }
: id VARS { $1 : $2 }
| id      { [$1] }

{
newtype Depth a = Depth a
newtype Dim a = Dim a

applyAttr :: (Int -> Int -> Bool -> Region)
          -> (Depth Int, Dim Int, Bool)
          -> Region
applyAttr constr (Depth d, Dim dim, irrefl) = constr d dim irrefl

data Specification
  = RegionDec String Region
  | SpecDec Spec [String]
  deriving (Show, Eq, Ord, Typeable, Data)

data Region
  = Forward Int Int Bool
  | Backward Int Int Bool
  | Centered Int Int Bool
  | Or Region Region
  | And Region Region
  | Var String
  deriving (Show, Eq, Ord, Typeable, Data)

data Spec = Spatial [Mod] Region
  deriving (Show, Eq, Ord, Typeable, Data)

data Mod
  = AtLeast
  | AtMost
  | ReadOnce
  deriving (Show, Eq, Ord, Typeable, Data)

--------------------------------------------------

data Token
  = TDoubleColon
  | TStar
  | TPlus
  | TEqual
  | TComma
  | TLParen
  | TRParen
  | TId String
  | TNum String
 deriving (Show)

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
      if (inp `hasPrefix` "stencil" || inp `hasPrefix` "region")
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
-- Comma hack: drop commas that are not separating numbers, in order to avoid need for 2-token lookahead.
lexer' (',':xs)
  | x':xs' <- dropWhile isSpace xs, not (isNumber x') = lexer' (x':xs')
  | otherwise                                         = addToTokens TComma xs
lexer' ('(':xs)                                        = addToTokens TLParen xs
lexer' (')':xs)                                        = addToTokens TRParen xs
lexer' (x:xs)
  | isLetter x                                        = aux TId $ \ c -> isAlphaNum c || c == '_'
  | isNumber x                                        = aux TNum isNumber
  | otherwise
     = failWith $ "Not an indentifier " ++ show x
 where
   aux f p = (f target :) `fmap` lexer' rest
     where (target, rest) = span p (x:xs)
lexer' x
    = failWith $ "Not a valid piece of stencil syntax " ++ show x

--------------------------------------------------

-- specParser :: String -> Either AnnotationParseError Specification
specParser :: AnnotationParser Specification
specParser src = do
 tokens <- lexer src
 parseSpec tokens >>= modValidate

-- Check whether modifiers are used correctly
modValidate :: Specification -> Either AnnotationParseError Specification
modValidate (SpecDec (Spatial mods r) vars) =
  do mods' <- modValidate' $ sort mods
     return $ SpecDec (Spatial mods' r) vars

  where    modValidate' [] = return $ []

           modValidate' (AtLeast : AtLeast : xs)
             = failWith "Duplicate 'atLeast' modifier; use at most one."

           modValidate' (AtMost : AtMost : xs)
             = failWith "Duplicate 'atMost' modifier; use at most one."

           modValidate' (ReadOnce : ReadOnce : xs)
             = failWith "Duplicate 'readOnce' modifier; use at most one."

           modValidate' (AtLeast : AtMost : xs)
             = failWith $ "Conflicting modifiers: cannot use 'atLeast' and "
                     ++ "'atMost' together"

           modValidate' (x : xs)
             = do xs' <- modValidate' xs
                  return $ x : xs'
modValidate x = return x

happyError :: [ Token ] -> Either AnnotationParseError a
happyError t = failWith $ "Could not parse specification at: " ++ show t

}
