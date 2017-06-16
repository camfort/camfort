{-# LANGUAGE FlexibleInstances #-}

module Camfort.ReprintSpec (spec) where

import Camfort.Functionality
import Camfort.Reprint
import Camfort.Helpers
import Camfort.Specification.Units.Monad (LiteralsOpt(LitMixed))
import qualified Data.ByteString.Char8 as B
import qualified Language.Fortran.Util.Position as FU

import System.FilePath
import System.Directory

import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec =
  describe "subtext function tests" $ do
    it "(unit test) first line of sample text" $
      subtext (1, 1) (1, 1) (2, 1) btext `shouldBe`
              (B.pack (text !! 0 ++ "\n"), B.pack (unlines . tail $ text))

    it "(unit test) second line of sample text" $
      subtext (1, 1) (2, 1) (3, 1) btext `shouldBe`
              (B.pack (text !! 1 ++ "\n"), B.pack (unlines . tail . tail $ text))

    it "(unit test) third line of sample text" $
      subtext (1, 1) (4, 1) (5, 1) btext `shouldBe`
              (B.pack (text !! 3 ++ "\n"), B.empty)

    it "(unit test) fourth line, middle, of sample text" $
      subtext (1, 1) (4, 2) (4, 5) btext `shouldBe`
              (B.pack "G H", B.pack " I J K L\n")

    it "(unit test) relative test (third line)" $
      subtext (3, 1) (5, 1) (6, 1) btext `shouldBe`
              (B.pack "  E F\n", B.pack " G H I J K L\n")

    it "(unit test) relative test (third line fragment)" $
      subtext (3, 1) (5, 1) (5, 4) btext `shouldBe`
              (B.pack "  E", B.pack " F\n G H I J K L\n")

    it "zero-length span at start yields empty string" $ property $
      \s -> subtext (0, 0) (0, 0) (0, 0) s == (B.empty, s)

    it "zero-length span yields empty substring" $ property $
      \(l,c) -> \s -> (fst $ subtext (l,c) (l,c) (l,c) s) == B.empty

{-
    it "takeBounds is the same as old one" $ property $
      \p   -> takeBoundsOld (FU.initPosition, p) btext
           == takeBounds    (FU.initPosition, p) btext

    it "takeBounds is the same as old one, with different start pos" $ property $
      \p   -> takeBoundsOld (FU.Position 0 2 2, unwrapPO p) btext
           == takeBounds    (FU.Position 0 2 2, unwrapPO p) btext
-}

    it "takeBounds test 1" $
      (fst $ takeBounds (FU.Position 0 2 2, FU.Position 0 5 2) btext)
        `shouldBe` (B.pack "A B")

    it "takeBounds test 2" $
      (fst $ takeBounds (FU.Position 0 2 2, FU.Position 0 1 3) btext)
        `shouldBe` (B.pack "A B C D\n")

    it "takeBound test 3" $
      (fst $ takeBounds (FU.Position 1 1 1, FU.Position 1 5 3) btext2)
        `shouldBe` (B.pack $ unlines $ take 3 text2)

    context "Integration test with synthesising a spec" $ do
       runIO $ unitsSynth ("tests" </> "fixtures" </> "simple.f90") []
         LitMixed False Nothing
         ("tests" </> "fixtures" </> "simple.f90.out") ATDefault
       actual <- runIO $ readFile ("tests" </> "fixtures" </> "simple.f90.out")
       expected <- runIO $ readFile ("tests" </> "fixtures" </> "simple.expected.f90")
       it "Unit synth" $ actual `shouldBe` expected

----

data PlusOne a = PlusOne { unwrapPO :: a } deriving Show

instance Arbitrary (PlusOne FU.Position) where
    arbitrary = do
      FU.Position offset col line <- arbitrary
      let col' = if line == 1 then col+1 else col
      return $ PlusOne $ FU.Position offset col' (line + 1)

instance Arbitrary FU.Position where
    arbitrary = do
      offset <- arbitrary `suchThat` (>0)
      line   <- arbitrary `suchThat` (\x -> x >= 1 && x <= (length text))
      col    <- choose (1, orOne $ length (text !! (line - 1)))
      return $ FU.Position offset col line

orOne x | x == 0    = 1
        | otherwise = x

-- Arbtirary ByteString
instance Arbitrary B.ByteString where
    arbitrary = do
      numLines <- choose (0, 3)
      return . B.pack . concat $ take numLines text

btext = B.pack . unlines $ text
text = ["A B C D"
       ,""
       ,"  E F"
       ," G H I J K L"]

btext2 = B.pack . unlines $ text2
text2 = ["A B C D"
       ,"E F"
       ,"G H"
       ,"I J K L"]

{-
-- Given a lower-bound and upper-bound pair of FU.Positions, split the
-- incoming SourceText based on the distanceF between the FU.Position pairs
takeBoundsOld :: (FU.Position, FU.Position) -> SourceText -> (SourceText, SourceText)
takeBoundsOld (l, u) = takeBounds' ((ll, lc), (ul, uc)) B.empty
  where (FU.Position _ lc ll) = l
        (FU.Position _ uc ul) = u
        takeBounds' ((ll, lc), (ul, uc)) tk inp  =
          if (ll == ul && lc == uc) || (ll > ul) then (B.reverse tk, inp)
          else
            case B.uncons inp of
              Nothing         -> (B.reverse tk, inp)
              Just ('\n', ys) -> takeBounds' ((ll+1, 1), (ul, uc))  (B.cons '\n' tk) ys
              Just (x, xs)    -> takeBounds' ((ll, lc+1), (ul, uc)) (B.cons x tk) xs
-}
