{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.Lexer where

import Data.Monoid (Alt(..))
import Data.Coerce
import qualified Data.Char as Char

import Control.Monad.State
import Control.Monad.Except

import Camfort.Specification.Hoare.Parser.Types

addToTokens :: Token -> String -> HoareSpecParser [Token]
addToTokens tok rest = do
 tokens <- lexer rest
 return $ tok : tokens

lexer :: String -> HoareSpecParser [Token]
lexer [] = return []
lexer (' ' : xs) = lexer xs
lexer ('\t' : xs) = lexer xs
lexer xs
  | Just (tok, rest) <- lexSymbol xs
  = addToTokens tok rest
lexer ('"' : xs) = do
  (tok, rest) <- lexQuoted xs
  addToTokens tok rest
lexer xs = do
  mname <- lexName xs
  case mname of
    Just (tok, rest) -> addToTokens tok rest
    Nothing -> throwError (LexError xs)


lexSymbol :: String -> Maybe (Token, String)
lexSymbol xs =
  let symbols =
        [ ("static_assert", TStaticAssert)
        , ("decl_aux", TDeclAux)
        , ("invariant", TInvariant)
        , ("post", TPost)
        , ("pre", TPre)
        , ("seq", TSeq)
        , ("&", TAnd)
        , ("|", TOr)
        , ("<->", TEquiv)
        , ("->", TImpl)
        , ("!", TNot)
        , ("t", TTrue)
        , ("f", TFalse)
        , ("(", TLParen)
        , (")", TRParen)
        , ("::", TDColon)
        ]

      tryMatch (symbol, tok) = (tok,) <$> stripPrefix symbol xs

      firstMatch = getAlt . mconcat . coerce

  in firstMatch (tryMatch <$> symbols)


lexQuoted :: String -> HoareSpecParser (Token, String)
lexQuoted input = do
  let
    go :: String -> StateT String HoareSpecParser String
    go ('"' : xs) = return xs
    go [] = throwError UnmatchedQuote
    go (c : xs) = do
      modify (c :)
      go xs

  (rest, expr) <- runStateT (go input) []
  return (TQuoted (reverse expr), rest)


isNameStartChar :: Char -> Bool
isNameStartChar c = Char.isLetter c || c == '_'

isNameChar :: Char -> Bool
isNameChar c =
  Char.isLetter c ||
  Char.isNumber c ||
  c == '_'

lexName :: String -> HoareSpecParser (Maybe (Token, String))
lexName xs =
  let (nm, rest) = span isNameChar xs
  in case nm of
    (n1 : _) | isNameStartChar n1 -> return (Just (TName nm, rest))
    _ -> return Nothing


stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix (p : refix) (s : tring)
  | p == s = stripPrefix refix tring
  | otherwise = Nothing
stripPrefix [] string = Just string
stripPrefix _ [] = Nothing
