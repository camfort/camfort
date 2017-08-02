{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.Lexer where

import Data.Monoid (Alt(..))
import Data.Coerce

import Control.Monad.State
import Control.Monad.Except

import Camfort.Specification.Hoare.Types

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
  (tok, rest) <- lexExpr xs
  addToTokens tok rest
lexer xs = throwError (LexError xs)


lexSymbol :: String -> Maybe (Token, String)
lexSymbol xs =
  let symbols =
        [ ("static_assert", TStaticAssert)
        , ("invariant", TInvariant)
        , ("post", TPost)
        , ("pre", TPre)
        , ("seq", TSeq)
        , ("<=", TLE)
        , (">=", TGE)
        , ("<", TLT)
        , (">", TGT)
        , ("=", TEquals)
        , ("&", TAnd)
        , ("|", TOr)
        , ("<->", TEquiv)
        , ("->", TImpl)
        , ("!", TNot)
        , ("(", TLParen)
        , (")", TRParen)
        ]

      tryMatch (symbol, tok) = (tok,) <$> stripPrefix symbol xs

      firstMatch = getAlt . mconcat . coerce

  in firstMatch (tryMatch <$> symbols)


lexExpr :: String -> HoareSpecParser (Token, String)
lexExpr input = do
  let
    go :: String -> StateT String HoareSpecParser String
    go ('"' : xs) = return xs
    go [] = throwError UnmatchedQuote
    go (c : xs) = do
      modify (c :)
      go xs

  (rest, expr) <- runStateT (go input) []
  return (TExpr (reverse expr), rest)


stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix (p : refix) (s : tring)
  | p == s = stripPrefix refix tring
  | otherwise = Nothing
stripPrefix [] string = Just string
