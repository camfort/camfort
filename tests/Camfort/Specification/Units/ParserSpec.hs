module Camfort.Specification.Units.ParserSpec (spec) where

import Camfort.Specification.Parser (runParser, SpecParseError)
import Camfort.Specification.Units.Parser (unitParser, UnitParseError)
import Camfort.Specification.Units.Parser.Types (UnitStatement)

import Test.Hspec

spec :: Spec
spec = do
  let shouldParseSameAs s s' =
        parseUnits s `shouldBe` parseUnits s'
      unitEquivalences =
        [ ("m * s", "m s")
        , ("m * s**2", "m s**2")
        , ("m * s/t", "m s/t")
        ]
  describe "Parsing Equivalence" $
    mapM_ (\(s1,s2) -> it (concat ["\"", s1, "\" is the same as \"", s2, "\""]) $
            s1 `shouldParseSameAs` s2) unitEquivalences

  describe "error messages" $ do
    invalidUnitSpecTestStr "invalid identifier character"
      "= unit ($) :: a"
      "Invalid character in identifier: '$'"
    invalidUnitSpecTestStr "invalid syntax"
      "= unit :: s"
      "Could not parse specification at: \"[]\"\n"

-- | Check that a unit specification does not parse, and that the
-- error string matches that provided.
--
-- Tests the @unitStr@ as is.
invalidUnitSpecTestStr :: String -- ^ Test description
                       -> String -- ^ Specification string
                       -> String -- ^ Expected error string
                       -> SpecWith ()
invalidUnitSpecTestStr description unitStr errStr =
  it description $ show (parse unitStr) `shouldBe` ("Left " ++ errStr)

parseUnits :: String -> Either (SpecParseError UnitParseError) UnitStatement
parseUnits = parse . (\s -> "= unit " ++ s ++ " :: a")

parse :: String -> Either (SpecParseError UnitParseError) UnitStatement
parse = runParser unitParser
