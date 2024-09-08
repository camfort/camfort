module Camfort.TestUtils where

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