{- |
Module      :  Camfort.Specification.Parser
Description :  Functionality common to all specification parsers.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

module Camfort.Specification.Parser
  (
   -- * Specification Parsers
    SpecParser(looksLikeASpec)
  , mkParser
  , runParser
  ) where

-- | Parser for specifications of type @r@ that may fail with error type @e@.
data SpecParser e r = SpecParser
  {
    -- | The underlying parser.
    parser         :: String -> Either e r
    -- | True if the text represents a (possibly syntactically invalid) specification.
  , looksLikeASpec :: String -> Bool
  }

-- | Run the given parser on a string to produce a specification (or a reason why it couldn't be parsed).
runParser :: SpecParser e r -> String -> Either e r
runParser = parser

-- | Define a specification parser.
mkParser :: (String -> Either e r) -- ^ Parser with error type @e@ and result type @r@.
         -> (String -> Bool)       -- ^ Predicate to indicate whether or not some
                                   -- text looks like (an attempt at) a specification.
         -> SpecParser e r
mkParser = SpecParser
