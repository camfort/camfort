{ -- -*- Mode: Haskell -*-

{-# LANGUAGE DeriveDataTypeable #-}
module Camfort.Specification.Units.Parser ( unitParser
                                     , UnitStatement(..)
                                     , UnitOfMeasure(..)
                                     , UnitPower(..)
                                     ) where

import Camfort.Analysis.CommentAnnotator
import Data.Data
import Data.List
import Data.Char (isLetter, isNumber, isAlphaNum, toLower)
}

%monad { Either AnnotationParseError } { >>= } { return }
%name parseUnit UNIT
%tokentype { Token }

%token
 unit  { TId "unit" }
 id    { TId $$ }
 one   { TNum "1" }
 num   { TNum $$ }
 ','   { TComma }
 '-'   { TMinus }
 '**'  { TExponentiation }
 '/'   { TDivision }
 '::'  { TDoubleColon }
 '='   { TEqual }
 '('   { TLeftPar }
 ')'   { TRightPar }

%left '/'
%left '**'
%%

UNIT :: { UnitStatement }
: unit UEXP VARIABLE_ANNOTATION { UnitAssignment $3 $2 }
| unit '::' id '=' UEXP { UnitAlias $3 $5 }

VARIABLE_ANNOTATION :: { Maybe [String] }
: '::' IDS { Just $2 }
| {-EMPTY-} { Nothing }

IDS :: { [String] }
: id ',' IDS   { $1 : $3 }
| id           { [$1] }

UEXP :: { UnitOfMeasure }
: UEXP_LEVEL1   { $1 }
| one           { Unitless }
| '(' one ')'   { Unitless }
| '(' ')'       { Unitless }

UEXP_LEVEL1 :: { UnitOfMeasure }
: UEXP_LEVEL1 UEXP_LEVEL2             { UnitProduct $1 $2 }
| UEXP '/' UEXP_LEVEL2                { UnitQuotient $1 $3 }
| UEXP_LEVEL2                         { $1 }

UEXP_LEVEL2 :: { UnitOfMeasure }
: UEXP_LEVEL2 '**' POW                { UnitExponentiation $1 $3 }
| '(' UEXP_LEVEL1 ')'                 { $2 }
| id                                  { UnitBasic $1 }

POW :: { UnitPower }
: SIGNED_NUM                          { UnitPowerInteger $1 }
| '(' SIGNED_NUM ')'                  { UnitPowerInteger $2 }
| '(' SIGNED_NUM '/' SIGNED_NUM ')'   { UnitPowerRational $2 $4 }

SIGNED_NUM :: { Integer }
: NUM       { read $1 }
| '-' NUM   { read $ '-' : $2 }

NUM :: { String }
: num   { $1 }
| one   { "1" }

{

data UnitStatement =
   UnitAssignment (Maybe [String]) UnitOfMeasure
 | UnitAlias String UnitOfMeasure
  deriving Data

instance Show UnitStatement where
  show (UnitAssignment (Just ss) uom) = "= unit (" ++ show uom ++ ") :: " ++ (intercalate "," ss)
  show (UnitAssignment Nothing uom) = "= unit (" ++ show uom ++ ")"
  show (UnitAlias s uom) = "= unit :: " ++ s ++ " = " ++ show uom

data UnitOfMeasure =
   Unitless
 | UnitBasic String
 | UnitProduct UnitOfMeasure UnitOfMeasure
 | UnitQuotient UnitOfMeasure UnitOfMeasure
 | UnitExponentiation UnitOfMeasure UnitPower
  deriving Data

instance Show UnitOfMeasure where
  show Unitless = "1"
  show (UnitBasic s) = s
  show (UnitProduct uom1 uom2) = show uom1 ++ " " ++ show uom2
  show (UnitQuotient uom1 uom2) = show uom1 ++ " / " ++ show uom2
  show (UnitExponentiation uom exp) = show uom ++ "** (" ++ show exp ++ ")"

data UnitPower =
   UnitPowerInteger Integer
 | UnitPowerRational Integer Integer
 deriving Data

instance Show UnitPower where
  show (UnitPowerInteger i) = show i
  show (UnitPowerRational i1 i2) = show i1 ++ "/" ++ show i2

data Token =
   TUnit
 | TComma
 | TDoubleColon
 | TExponentiation
 | TDivision
 | TMinus
 | TEqual
 | TLeftPar
 | TRightPar
 | TId String
 | TNum String
 deriving (Show)

lexer :: String -> Either AnnotationParseError [ Token ]
lexer (c:xs)
  | c `elem` ['=', '!', '>'] = lexer' xs
  | otherwise = Left NotAnnotation

addToTokens :: Token -> String -> Either AnnotationParseError [ Token ]
addToTokens tok rest = do
 tokens <- lexer' rest
 return $ tok : tokens

lexer' :: String -> Either AnnotationParseError [ Token ]
lexer' [] = Right []
lexer' ['\n']  = Right []
lexer' ['\r', '\n']  = Right []
lexer' ['\r']  = Right [] -- windows
lexer' (' ':xs) = lexer' xs
lexer' ('\t':xs) = lexer' xs
lexer' (':':':':xs) = addToTokens TDoubleColon xs
lexer' ('*':'*':xs) = addToTokens TExponentiation xs
lexer' (',':xs) = addToTokens TComma xs
lexer' ('/':xs) = addToTokens TDivision xs
lexer' ('-':xs) = addToTokens TMinus xs
lexer' ('=':xs) = addToTokens TEqual xs
lexer' ('(':xs) = addToTokens TLeftPar xs
lexer' (')':xs) = addToTokens TRightPar xs
lexer' (x:xs)
 | isLetter x = aux (\c -> isAlphaNum c || c `elem` ['\'','_','-']) TId
 | isNumber x = aux isNumber TNum
 | otherwise = Left NotAnnotation -- failWith $ "Not valid unit syntax at " ++ show (x:xs)
 where
   aux p cons =
     let (target, rest) = span p xs
     in lexer' rest >>= (\tokens -> return $ cons (x:target) : tokens)

unitParser :: String -> Either AnnotationParseError UnitStatement
unitParser src = do
 tokens <- lexer $ map toLower src
 parseUnit tokens

happyError :: [ Token ] -> Either AnnotationParseError a
happyError t = Left NotAnnotation -- failWith $ "Could not parse specification at: " ++ show t

}
