{ -- -*- Mode: Haskell -*-
{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
module Camfort.Analysis.StencilSpecification.Grammar
( specParser, Specification(..), Region(..), Spec(..), Mod(..), lexer ) where

import Data.Char (isLetter, isNumber, isAlphaNum, toLower, isAlpha, isSpace)
import Data.List (intersect, sort, isPrefixOf)
import Data.Data

import Camfort.Analysis.CommentAnnotator
import Camfort.Analysis.StencilSpecification.Syntax (showL)

}

%monad { Either AnnotationParseError } { >>= } { return }
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
: REGIONDEC                 { RegionDec (fst $1) (snd $1) }
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
| APPROXMODS MODS REGION         { Spatial ($1 ++ $2) $3 }
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

-- Even though multiple approx mods is not allowed
-- allow them to be parsed so that the validator can
-- report a nice error if the user supplies more than one
APPROXMODS :: { [Mod] }
: APPROXMOD APPROXMODS { $1 : $2 }
| APPROXMOD            { [$1] }

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
  = RegionDec String Region
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
  = AtLeast
  | AtMost
  | Irreflexive [Int]
  | ReadOnce
  | Reflexive [Int]
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
lexer input =
  -- First test to see if the input looks like an actual
  -- specification of either a stencil or region
  if (input `hasPrefix` "stencil" || input `hasPrefix` "region")
  then lexer' input
  else Left NotAnnotation

  where
    hasPrefix []       str = False
    hasPrefix (' ':xs) str = hasPrefix xs str
    hasPrefix xs       str = isPrefixOf str xs

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

           modValidate' (Reflexive ds : Reflexive ds' : xs)
             = failWith "Duplicate 'reflexive' modifier; use at most one."

           modValidate' (Irreflexive ds : Irreflexive ds' : xs)
             = failWith "Duplicate 'irreflexive' modifier; use at most one."

           modValidate' (AtLeast : AtLeast : xs)
             = failWith "Duplicate 'atLeast' modifier; use at most one."

           modValidate' (AtMost : AtMost : xs)
             = failWith "Duplicate 'atMost' modifier; use at most one."

           modValidate' (ReadOnce : ReadOnce : xs)
             = failWith "Duplicate 'readOnce' modifier; use at most one."

           modValidate' (AtLeast : AtMost : xs)
             = failWith $ "Conflicting modifiers: cannot use 'atLeast' and "
                     ++ "'atMost' together"

           modValidate' (Irreflexive ds : xs)
             = case inconsistentReflexives ds xs of
                 [] ->  do xs' <- modValidate' xs
                           return $ Irreflexive ds : xs'
                 ds' -> failWith $ "Conflicting modifiers: stencil marked as "
                                ++ "both irreflexive and reflexive in "
                                ++ "dimensions = " ++ showL ds'
           modValidate' (x : xs)
             = do xs' <- modValidate' xs
                  return $ x : xs'

           -- Find reflexive dimenions which overlap with the first parameter
           inconsistentReflexives ds [] = []
           inconsistentReflexives ds (Reflexive ds' : _) = intersect ds ds'
           inconsistentReflexives ds (m : ms) = inconsistentReflexives ds ms
modValidate x = return x

happyError :: [ Token ] -> Either AnnotationParseError a
happyError t = failWith $ "Could not parse specification at: " ++ show t

}
