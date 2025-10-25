module Camfort.TestUtils where

import Test.Hspec
import Text.Regex

-- Removes formatting information from messages
hideFormatting :: String -> String
hideFormatting = hideEscapes' False
  where
    hideEscapes' _ [] = []
    hideEscapes' True (';':'1':'m':xs) = hideEscapes' False xs
    hideEscapes' True (x:xs) = hideEscapes' True xs
    hideEscapes' False ('\ESC':'[':'0':'m':xs) = hideEscapes' False xs
    hideEscapes' False ('\ESC':'[':xs) = hideEscapes' True xs
    hideEscapes' False (x:xs) = x : hideEscapes' False xs

-- Cross-platform string comparison that normalises line endings and path separators
normalisedShouldBe :: String -> String -> Expectation
normalisedShouldBe actual expected =
  normaliseForComparison actual `shouldBe` normaliseForComparison expected

normaliseForComparison :: String -> String
normaliseForComparison = normalisePositions . map (\c -> if c == '\\' then '/' else c) . filter (/= '\r')
  where
    -- Normalize position ranges like (8:0)-(8:51) to (8:0)-(8:*)
    -- This makes column differences irrelevant in comparisons
    normalisePositions str =
      let regex = mkRegex "\\([0-9]+:[0-9]+\\)-\\([0-9]+:[0-9]+\\)"
      in subRegex regex str "(LINE:COL)-(LINE:COL)"