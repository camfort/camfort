module Camfort.Specification.ParserSpec (spec) where

import Data.Either (isLeft)

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Test

import Camfort.Specification.Parser (looksLikeASpec, mkParser, runParser, SpecParser)

trivialParser :: SpecParser String String
trivialParser = mkParser Right ["test"]

canParseTo :: SpecParser String String -> String -> String -> Expectation
canParseTo p s e = runParser p s `shouldBe` Right e

parsesTriviallyTo :: String -> String -> Expectation
parsesTriviallyTo = canParseTo trivialParser

doesNotParse :: String -> Expectation
doesNotParse s = runParser trivialParser s `shouldSatisfy` isLeft

spec :: Test.Spec
spec = do
  describe "specification characters" $ do
    let expectedSupportedChars = "!=<>"
    mapM_
      (\c -> it ("supports " ++ show c) ((c:" test") `parsesTriviallyTo` "test"))
      expectedSupportedChars
    it "allows whitespace before specification character" $
      "  = test" `parsesTriviallyTo` "test"
    it "requires a specification character" $
      doesNotParse ""
    it "does not accept specifications starting with invalid characters" $
      doesNotParse "c test"
  describe "specification keywords" $ do
    let testParser = mkParser Right []
    it "does not require any to be present (parsing)" $
      canParseTo testParser "= test" "test"
    it "does not require any to be present (checking)" $
      looksLikeASpec testParser "= test" `shouldBe` True
