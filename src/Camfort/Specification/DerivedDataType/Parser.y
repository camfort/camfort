{ -- -*- Mode: Haskell -*-
{-# LANGUAGE DeriveDataTypeable #-}

module Camfort.Specification.DerivedDataType.Parser
  ( ddtParser
  , DDTParseError
  , DDTStatement(..)
  ) where

import Control.Monad.Except (throwError)
import Data.Char (isLetter, isNumber, isAlphaNum, toLower)
import Data.Data (Data)
import Camfort.Specification.Parser (mkParser, SpecParser)

}

%monad { DDTSpecParser } { >>= } { return }
%name parseDDT DDT
%tokentype { Token }

%token
 ddt      { TId "ddt" }
 dim      { TId "dim" }
 id       { TId $$ }
 num      { TNum $$ }
 ','      { TComma }
 '::'     { TDoubleColon }
 '='      { TEqual }
 '=>'     { TArrow }
 '('      { TLeftPar }
 ')'      { TRightPar }

%left '*'
%left '/'
%left '**'
%%

DDT :: { DDTStatement }
: ddt id '(' LABELS ')' '::' VARDIMS { DDTSt $2 $4 $7 }

VARDIMS :: { [(String, Int)] }
: VARDIM ',' VARDIMS   { $1 : $3 }
| VARDIM               { [$1] }

VARDIM :: { (String, Int) }
: id '(' dim '=' num ')'  { ($1, read $5) }

LABELS :: { [(String, String)] }
: LABEL ',' LABELS     { $1 : $3 }
| LABEL                { [$1] }

LABEL :: { (String, String) }
: num '=>' id          { ($1, $3) }

{

data DDTStatement
  = DDTSt { ddtStTypeName :: String
          , ddtStLabels :: [(String, String)]
          , ddtStVarDims :: [(String, Int)] }
  deriving (Data, Eq, Show)

data DDTParseError
  -- | Not a valid identifier character.
  = NotAnIdentifier Char
  -- | Tokens do not represent a syntactically valid specification.
  | CouldNotParseSpecification [Token]
  deriving (Eq)

instance Show DDTParseError where
  show (CouldNotParseSpecification ts) =
    "Could not parse specification at: \"" ++ show ts ++ "\"\n"
  show (NotAnIdentifier c) = "Invalid character in identifier: " ++ show c

notAnIdentifier :: Char -> DDTParseError
notAnIdentifier = NotAnIdentifier

couldNotParseSpecification :: [Token] -> DDTParseError
couldNotParseSpecification = CouldNotParseSpecification

type DDTSpecParser a = Either DDTParseError a

data Token =
   TComma
 | TDoubleColon
 | TArrow
 | TEqual
 | TLeftPar
 | TRightPar
 | TId String
 | TNum String
 deriving (Show, Eq)

addToTokens :: Token -> String -> DDTSpecParser [ Token ]
addToTokens tok rest = do
 tokens <- lexer rest
 return $ tok : tokens

lexer :: String -> DDTSpecParser [ Token ]
lexer [] = Right []
lexer ['\n']  = Right []
lexer ['\r', '\n']  = Right []
lexer ['\r']  = Right [] -- windows
lexer (' ':xs) = lexer xs
lexer ('\t':xs) = lexer xs
lexer (':':':':xs) = addToTokens TDoubleColon xs
lexer ('=':'>':xs) = addToTokens TArrow xs
lexer (',':xs) = addToTokens TComma xs
lexer ('=':xs) = addToTokens TEqual xs
lexer ('(':xs) = addToTokens TLeftPar xs
lexer (')':xs) = addToTokens TRightPar xs
lexer (x:xs)
 | isLetter x || x == '\'' = aux (\ c -> isAlphaNum c || c `elem` ['\'','_','-'])
                                 TId
 | isNumber x              = aux isNumber TNum
 | otherwise
     = throwError $ notAnIdentifier x
 where
   aux p cons =
     let (target, rest) = span p xs
     in lexer rest >>= (\tokens -> return $ cons (x:target) : tokens)

ddtParser :: SpecParser DDTParseError DDTStatement
ddtParser = mkParser (\src -> do
                          tokens <- lexer $ map toLower src
                          parseDDT tokens) ["ddt"]

happyError :: [ Token ] -> DDTSpecParser a
happyError = throwError . couldNotParseSpecification

}
