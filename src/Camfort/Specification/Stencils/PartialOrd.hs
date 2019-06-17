{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Data.PartialOrd
Description : Provides the PartialOrd Typeclass.
Copyright   : (c) 2016 Moritz Schulte
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module provides the `PartialOrd' typeclass suitable for types
admitting a partial order.

Along with the `PartialOrd' typeclass and some utility functions for
working with partially ordered types, it exports implementations for
the numeric types several numeric types, lists and sets.
-}

-- Code imported into camfort to avoid build-dependency hell with
-- unmaintained upstream package.

module Camfort.Specification.Stencils.PartialOrd
  ( PartialOrd(..)
  , maxima, minima
  , elem, notElem
  , nub ) where

import Data.Bool
import Data.Maybe
import Prelude (Int, Integer, Float, Double, ($), Integral)
import qualified Data.Ord as Ord
import qualified Data.Eq as Eq
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable

class PartialOrd a where

  -- | Less-than-or-equal relation.
  (<=) :: a -> a -> Bool

  -- | Bigger-than-or-equal relation. Defined in terms of `<='.
  (>=) :: a -> a -> Bool
  a >= a' = a' <= a

  -- | Equality relation. Defined in terms of `<='.
  (==) :: a -> a -> Bool
  a == a' = a <= a' && a' <= a

  -- | Inequality relation. Defined in terms of `=='.
  (/=) :: a -> a -> Bool
  a /= a' = not (a == a')

  -- | Less-than relation relation. Defined in terms of `<=' and `/='.
  (<) :: a -> a -> Bool
  a < a' = a <= a' && (a /= a')

  -- | Bigger-than relation. Defined in terms of `<=` and `/='.
  (>) :: a -> a -> Bool
  a > a' = a' <= a && (a /= a')

  -- | Compare function, returning either `Just' an `Ordering' or
  -- `Nothing'.
  compare :: a -> a -> Maybe Ord.Ordering
  compare a a' = if | a == a'   -> Just Ord.EQ
                    | a <= a'   -> Just Ord.LT
                    | a >= a'   -> Just Ord.GT
                    | otherwise -> Nothing

  {-# MINIMAL (<=) #-}

-- | Derive the partial order from the total order for the following
-- types:
instance PartialOrd Int where
  (<=) = (Ord.<=)

instance PartialOrd Integer where
  (<=) = (Ord.<=)

instance PartialOrd Double where
  (<=) = (Ord.<=)

instance PartialOrd Float where
  (<=) = (Ord.<=)

-- | Define the partial order in terms of the subset relation.
instance (Ord.Ord a) => PartialOrd (Set.Set a) where
  (<=) = Set.isSubsetOf

-- | Define the partial order in terms of the sublist relation.
instance PartialOrd a => PartialOrd [a] where
  (<=) = isSublistOf

-- | Return True if the first list is a sublist of the second list.
isSublistOf :: PartialOrd a => [a] -> [a] -> Bool
isSublistOf [] _ = True
isSublistOf (a:as) a' = a `elem` a' && as `isSublistOf` a'

-- | Compute the list of all elements that are not less than any other
-- element in the list.
maxima :: PartialOrd a => [a] -> [a]
maxima as = nub $ extrema (<=) as

-- | Compute the list of all elements that are not bigger than any
-- other element in the list.
minima :: PartialOrd a => [a] -> [a]
minima as = nub $ extrema (>=) as

extrema :: PartialOrd a => (a -> a -> Bool) -> [a] -> [a]
extrema f as = List.filter isExtremal as
  where isExtremal a =
          -- Return true if there exists no a' in as \ {a} such that
          --   a `f` a'.
          let as' = List.filter (/= a) as
          in not (Foldable.any (a `f`) as')

-- | Version of the traditional elem function using the PartialOrd
-- notion of equality.
elem :: (PartialOrd a, Foldable.Foldable t) => a -> t a -> Bool
elem x xs = Foldable.any (x ==) xs

-- | Version of the traditional notElem function using the PartialOrd
-- notion of equality.
notElem :: (PartialOrd a, Foldable.Foldable t) => a -> t a -> Bool
notElem x xs = not $ elem x xs

-- | Version of the traditional nub function using the PartialOrd
-- notion of equality.
nub :: PartialOrd a => [a] -> [a]
nub as = List.reverse $ Foldable.foldl' collect [] as
  where collect uniques a =
          if a `elem` uniques
          then uniques
          else a : uniques
