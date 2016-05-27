{ -- -*- Mode: Haskell -*-
module Camfort.Extensions.UnitParser ( unitParser
                                     , UnitStatement(..)
                                     , UnitOfMeasure(..)
                                     , UnitPower(..) 
                                     ) where

import Data.Char (isLetter, isNumber, isAlphaNum, toLower)
}

%monad { Maybe } { >>= } { return } 
%name parseUnit UNIT
%tokentype { Token }

%token
 unit  { TId "unit" }
 id    { TId $$ }
 one   { TNum "1" }
 num   { TNum $$ }
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

VARIABLE_ANNOTATION :: { Maybe String }
: '::' id { Just $2 }
| {-EMPTY-} { Nothing }

UEXP :: { UnitOfMeasure }
: UEXP_LEVEL1   { $1 }
| one           { Unitless }
| '(' ')'       { Unitless }

UEXP_LEVEL1 :: { UnitOfMeasure }
: UEXP_LEVEL1 UEXP_LEVEL2             { UnitProduct $1 $2 }
| UEXP_LEVEL1 '/' UEXP_LEVEL2         { UnitQuotient $1 $3 }
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
   UnitAssignment (Maybe String) UnitOfMeasure
 | UnitAlias String UnitOfMeasure

instance Show UnitStatement where
  show (UnitAssignment (Just s) uom) = "= unit (" ++ show uom ++ ") :: " ++ s 
  show (UnitAssignment Nothing uom) = "= unit (" ++ show uom ++ ")"
  show (UnitAlias s uom) = "= unit :: " ++ s ++ " = " ++ show uom

data UnitOfMeasure = 
   Unitless
 | UnitBasic String
 | UnitProduct UnitOfMeasure UnitOfMeasure
 | UnitQuotient UnitOfMeasure UnitOfMeasure
 | UnitExponentiation UnitOfMeasure UnitPower

instance Show UnitOfMeasure where
  show Unitless = "1"
  show (UnitBasic s) = s
  show (UnitProduct uom1 uom2) = show uom1 ++ " " ++ show uom2
  show (UnitQuotient uom1 uom2) = show uom1 ++ " / " ++ show uom2
  show (UnitExponentiation uom exp) = show uom ++ "** (" ++ show exp ++ ")"

data UnitPower =
   UnitPowerInteger Integer
 | UnitPowerRational Integer Integer

instance Show UnitPower where
  show (UnitPowerInteger i) = show i
  show (UnitPowerRational i1 i2) = show i1 ++ "/" ++ show i2

data Token = 
   TUnit
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

lexer :: String -> Maybe [ Token ]
lexer ('=':xs) = lexer' xs
lexer _ = Nothing

addToTokens :: Token -> String -> Maybe [ Token ]
addToTokens tok rest = do
 tokens <- lexer' rest 
 return $ tok : tokens

lexer' :: String -> Maybe [ Token ]
lexer' [] = Just []
lexer' (' ':xs) = lexer' xs
lexer' ('\t':xs) = lexer' xs
lexer' (':':':':xs) = addToTokens TDoubleColon xs
lexer' ('*':'*':xs) = addToTokens TExponentiation xs
lexer' ('/':xs) = addToTokens TDivision xs
lexer' ('-':xs) = addToTokens TMinus xs
lexer' ('=':xs) = addToTokens TEqual xs
lexer' ('(':xs) = addToTokens TLeftPar xs
lexer' (')':xs) = addToTokens TRightPar xs
lexer' (x:xs)
 | isLetter x = aux (\c -> isAlphaNum c || c `elem` ['\'','_','-']) TId
 | isNumber x = aux isNumber TNum
 | otherwise = Nothing
 where
   aux p cons = 
     let (target, rest) = span p xs
     in lexer' rest >>= (\tokens -> return $ cons (x:target) : tokens)
lexer' _ = Nothing

unitParser :: String -> Maybe UnitStatement
unitParser src = do
 tokens <- lexer $ map toLower src
 parseUnit tokens

happyError :: [ Token ] -> Maybe a
happyError _ = Nothing

}
