{ -- -*- Mode: Haskell -*-
{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
module Camfort.Analysis.StencilSpecification.Grammar
( specParser, Specification(..), Region(..), Spec(..), Mod(..), lexer ) where

import Data.Char (isLetter, isNumber, isAlphaNum, toLower, isAlpha, isSpace)
import Data.Data

}

%monad { Maybe } { >>= } { return }
%name parseSpec SPEC
%tokentype { Token }
%token
  stencil     { TId "stencil" }
  region      { TId "region" }
  readOnce    { TId "readOnce" }
  reflexive   { TId "reflexive" }
  irreflexive { TId "irreflexive" }
  atMost      { TId "atMost" }
  atLeast     { TId "atLeast" }
  dims        { TId "dims" }
  dim         { TId "dim" }
  depth       { TId "depth" }
  forward     { TId "forward" }
  backward    { TId "backward" }
  centered    { TId "centered" }
  dependency  { TId "dependency" }
  mutual      { TId "mutual" }
  id          { TId $$ }
  num         { TNum $$ }
  '+'         { TPlus }
  '*'         { TStar }
  '::'        { TDoubleColon }
  '='         { TEqual }
  '('         { TLParen }
  ')'         { TRParen }
  ','         { TComma }

%left '+'
%left '*'

%%

SPEC :: { Specification }
: REGIONDEC                 { RegionDec $1 }
| stencil SPECDEC '::' VARS { SpecDec $2 $4 }

REGIONDEC :: { (String, Region) }
: region id '=' REGION { ($2, $4) }

REGION ::                            { Region }
: forward  '(' depth '=' num dim '=' num ')' { Forward  (read $5) (read $8) }
| backward '(' depth '=' num dim '=' num ')' { Backward (read $5) (read $8) }
| centered '(' depth '=' num dim '=' num ')' { Centered (read $5) (read $8) }
| REGION '+' REGION                  { Or $1 $3 }
| REGION '*' REGION                  { And $1 $3 }
| '(' REGION ')'                     { $2 }
| id                                 { Var $1 }

SPECDEC :: { Spec }
: dependency '(' VARS ')'        { Temporal $3 False }
| dependency '(' VARS ')' mutual { Temporal $3 True }
| APPROXMOD MODS REGION          { Spatial ($1: $2) $3 }
| MODS REGION                    { Spatial $1 $2 }
| APPROXMOD REGION               { Spatial [$1] $2 }
| REGION                         { Spatial [] $1 }

MODS :: { [Mod] }
: MOD MODS { $1 : $2 }
| MOD      { [$1] }

MOD :: { Mod }
: readOnce                          { ReadOnce }
| reflexive '(' dims '=' DIMS ')'   { Reflexive $5 }
| irreflexive '(' dims '=' DIMS ')' { Irreflexive $5 }

APPROXMOD :: { Mod }
: atMost                    { AtMost }
| atLeast                   { AtLeast }

DIMS :: { [Int] }
: num ',' DIMS { read $1 : $3 }
| num          { [read $1] }

VARS :: { [String] }
: id VARS { $1 : $2 }
| id      { [$1] }

{

data Specification
  = RegionDec (String, Region)
  | SpecDec Spec [String]
  deriving (Show, Eq, Ord, Typeable, Data)

data Region
  = Forward Int Int
  | Backward Int Int
  | Centered Int Int
  | Or Region Region
  | And Region Region
  | Var String
  deriving (Show, Eq, Ord, Typeable, Data)

data Spec
  = Spatial [Mod] Region
  | Temporal [String] Bool
  deriving (Show, Eq, Ord, Typeable, Data)

data Mod
  = ReadOnce
  | Reflexive [Int]
  | Irreflexive [Int]
  | AtMost
  | AtLeast
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

addToTokens :: Token -> String -> Maybe [ Token ]
addToTokens tok rest = do
 tokens <- lexer rest
 return $ tok : tokens

lexer :: String -> Maybe [ Token ]
lexer []                                              = Just []
lexer (' ':xs)                                        = lexer xs
lexer ('\t':xs)                                       = lexer xs
lexer (':':':':xs)                                    = addToTokens TDoubleColon xs
lexer ('*':xs)                                        = addToTokens TStar xs
lexer ('+':xs)                                        = addToTokens TPlus xs
lexer ('=':xs)                                        = addToTokens TEqual xs
-- Comma hack: drop commas that are not separating numbers, in order to avoid need for 2-token lookahead.
lexer (',':xs)
  | x':xs' <- dropWhile isSpace xs, not (isNumber x') = lexer (x':xs')
  | otherwise                                         = addToTokens TComma xs
lexer ('(':xs)                                        = addToTokens TLParen xs
lexer (')':xs)                                        = addToTokens TRParen xs
lexer (x:xs)
  | isLetter x                                        = aux TId $ \ c -> isAlphaNum c || c == '_'
  | isNumber x                                        = aux TNum isNumber
  | otherwise                                         = Nothing
 where
   aux f p = (f target :) `fmap` lexer rest
     where (target, rest) = span p (x:xs)
lexer _                                               = Nothing

--------------------------------------------------

specParser :: String -> Maybe Specification
specParser src = do
 tokens <- lexer src
 parseSpec tokens

happyError :: [ Token ] -> Maybe a
happyError _ = Nothing

}
