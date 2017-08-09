{- |
Module      :  Camfort.Specification.Parser
Description :  Functionality common to all specification parsers.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Camfort.Specification.Parser
  (
  -- * Specification Parsers
    SpecParser
  , looksLikeASpec
  , mkParser
  , runParser
  -- ** Errors
  , SpecParseError
  , parseError
  ) where

import           Control.Monad.Except (throwError)
import           Control.Exception (Exception(..))
import           Data.Data
import           Data.List            (isPrefixOf)
import qualified Data.Text            as T

data SpecParseError e
  = ParseError e
  | InvalidSpecificationCharacter Char
  | MissingSpecificationCharacter
  deriving (Eq, Typeable, Data)

instance (Show e) => Show (SpecParseError e) where
  show (InvalidSpecificationCharacter c) =
    "Invalid character at start of specification: " ++ show c
  show MissingSpecificationCharacter = "missing start of specification"
  show (ParseError e) = show e

instance Exception e => Exception (SpecParseError e) where
  displayException (InvalidSpecificationCharacter c) =
    "Invalid character at start of specification: " ++ show c
  displayException MissingSpecificationCharacter = "missing start of specification"
  displayException (ParseError e) = displayException e

-- | Embed an error as a specification parse error.
parseError :: e -> SpecParseError e
parseError = ParseError

invalidSpecificationCharacter :: Char -> SpecParseError e
invalidSpecificationCharacter = InvalidSpecificationCharacter

missingSpecificationCharacter :: SpecParseError e
missingSpecificationCharacter = MissingSpecificationCharacter

-- | Parser for specifications of type @r@ that may fail with error type @e@.
data SpecParser e r = SpecParser
  {
    -- | The underlying parser.
    parser       :: String -> Either e r
    -- | A list of keywords that indicate the type of specification (e.g., @"stencil"@ or @"access"@).
  , specKeywords :: [String]
  }

-- | Does the character indicate the start of an abritrary specification?
--
-- These characters are used to help distinguish specifications
-- from normal comments.
isSpecStartChar :: Char -> Bool
isSpecStartChar = (`elem` "=!<>")

-- | Run the given parser on a string to produce a specification
-- (or a reason why it couldn't be parsed).
runParser :: SpecParser e r -> String -> Either (SpecParseError e) r
runParser p s = case stripInitial s of
                  Right s' -> case parser p s' of
                                Left  e -> throwError $ parseError e
                                Right r -> pure r
                  Left e   -> throwError e
  where stripInitial = stripAnnChar . stripLeadingWhiteSpace
        stripAnnChar [] =
          throwError missingSpecificationCharacter
        stripAnnChar (c:cs) | isSpecStartChar c = pure (stripLeadingWhiteSpace cs)
                            | otherwise         =
                                throwError $ invalidSpecificationCharacter c

-- | Define a specification parser.
mkParser :: (String -> Either e r) -- ^ Parser with error type @e@ and result type @r@.
         -> [String]               -- ^ Keywords that indicate the type of specification.
         -> SpecParser e r
mkParser = SpecParser

-- | Remove any whitespace characters at the beginning of the string.
stripLeadingWhiteSpace :: String -> String
stripLeadingWhiteSpace = T.unpack . T.strip . T.pack

-- | Check if a comment is probably an attempt at a specification
-- that can be parsed by the given parser.
looksLikeASpec :: SpecParser e r -> String -> Bool
looksLikeASpec p text
  | length (stripLeadingWhiteSpace text) >= 2 =
  case stripLeadingWhiteSpace text of
    -- Check the leading character is '=' for specification
    c:cs -> isSpecStartChar c && testAnnotation cs
    _    -> False
  | otherwise = False
  where
    testAnnotation inp = case specKeywords p of
                           [] -> True
                           ks -> any (inp `hasPrefix`) ks
    hasPrefix []       _   = False
    hasPrefix (' ':xs) str = hasPrefix xs str
    hasPrefix xs       str = str `isPrefixOf` xs
