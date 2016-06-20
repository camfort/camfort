{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module Camfort.Analysis.StencilSpecification.InferenceBackend where

import Prelude hiding (sum)
import Data.Generics.Uniplate.Operations
import Data.List hiding (sum)
import Data.Data
import Control.Arrow ((***))

import Camfort.Helpers
import Camfort.Helpers.Vec

import Unsafe.Coerce

import Camfort.Analysis.StencilSpecification.Syntax

{- Spans are a pair of a lower and upper bound -}
type Span a = (a, a)
mkTrivialSpan a = (a, a)

inferFromIndices :: VecList Int -> Specification
inferFromIndices (VL ixs) =
    setLinearity (fromBool mult) (Specification . Left . inferSpec $ ixs')
      where (ixs', mult) = hasDuplicates ixs
            inferSpec :: Permutable n => [Vec n Int] -> Result Spatial
            inferSpec = fromRegionsToSpec . inferMinimalVectorRegions

-- Same as inferFromIndices but don't do any linearity checking
-- (defaults to NonLinear). This is used when the front-end does
-- the linearity check first as an optimimsation.
inferFromIndicesWithoutLinearity :: VecList Int -> Specification
inferFromIndicesWithoutLinearity (VL ixs) =
    Specification . Left . inferSpec $ ixs
      where inferSpec :: Permutable n => [Vec n Int] -> Result Spatial
            inferSpec = fromRegionsToSpec . inferMinimalVectorRegions

-- Removes any 'reflexive' specs that are overlapped by a directional spec
-- somewhere else [in theory a lot of these won't get generated in the first
-- place, but when specs are combined there can be overlapping reflexivities in
-- different parts]
simplifyRefl :: Spatial -> Spatial
simplifyRefl (Spatial lin irdims rdims (Sum ss)) =
  Spatial lin (irdims \\ overlapped) (rdims \\ overlapped) (Sum ss)
    where
      overlapped = rdimsS ++ rdimsF ++ rdimsB
      rdimsS = [d | (Centered _ d)  <- universeBi ss::[Region],
                                 d' <- rdims, d == d']

      rdimsF = [d | (Forward  _ d)  <- universeBi ss::[Region],
                                 d' <- rdims, d == d']

      rdimsB = [d | (Backward  _ d)  <- universeBi ss::[Region],
                                  d' <- rdims, d == d']

fromRegionsToSpec :: [Span (Vec n Int)] -> Result Spatial
fromRegionsToSpec sps = fmap simplifyRefl result
  where
    result = foldr (\x y -> sum (toSpecND x) y) zero sps
    -- Compute a full upper using a bounding box
    -- Probably we don't need this anymore: it should agree
    -- with the upper bound computed using `prod`
    -- upper  = toSpecND $ foldl1 spanBoundingBox sps

-- toSpecND converts an n-dimensional region into a (list of) specification
toSpecND :: Span (Vec n Int) -> Result Spatial
toSpecND = toSpecPerDim 1
  where
   -- convert the region one dimension at a time.
   toSpecPerDim :: Int -> Span (Vec n Int) -> Result Spatial
   toSpecPerDim d (Nil, Nil)             = one
   toSpecPerDim d (Cons l ls, Cons u us) =
     prod (toSpec1D d l u) (toSpecPerDim (d + 1) (ls, us))

-- toSpec1D takes a dimension identifier, a lower and upper bound of a region in
-- that dimension, and builds the simple directional spec.
toSpec1D :: Dimension -> Int -> Int -> Result Spatial
toSpec1D dim l u
    | l == absoluteRep || u == absoluteRep =
        Exact emptySpatialSpec -- the "one" element wrt. "prod"

    | l == 0 && u == 0 =
        Exact $ Spatial NonLinear [] [dim] (Sum [Product []])
    | l < 0 && u == 0 =
        Exact $ Spatial NonLinear [] [] (Sum [Product [Backward (abs l) dim]])

    | l < 0 && u == (-1) =
        Exact $ Spatial NonLinear [dim] [] (Sum [Product [Backward (abs l) dim]])

    | l == 0 && u > 0 =
        Exact $ Spatial NonLinear [] [] (Sum [Product [Forward u dim]])

    | l == 1 && u > 0 =
        Exact $ Spatial NonLinear [dim] [] (Sum [Product [Forward u dim]])

    -- from the above constraints above, l and u are neither 0, 1 or -1.
    -- Represents a non-continguous region
    | l==u = upperBound $ Spatial NonLinear [] [] (Sum [Product
                            [if l > 0 then Forward u dim else Backward u dim]])

    | l < 0 && u > 0 && (abs l == u) =
        Exact $ Spatial NonLinear [] [] (Sum [Product [Centered u dim]])

    | l < 0 && u > 0 && (abs l /= u) =
        Exact $ Spatial NonLinear [] [] (Sum [Product [Backward (abs l) dim],
                                          Product [Forward u dim]])
    -- Represents a non-contiguous region
    | otherwise =
        upperBound $ Spatial NonLinear [] [] (Sum [Product
                        [if l > 0 then Forward u dim else Backward u dim]])

{- Normalise a span into the form (lower, upper) based on the first index -}
normaliseSpan :: Span (Vec n Int) -> Span (Vec n Int)
normaliseSpan (Nil, Nil)
    = (Nil, Nil)
normaliseSpan (a@(Cons l1 ls1), b@(Cons u1 us1))
    | l1 <= u1  = (a, b)
    | otherwise = (b, a)

{- `spanBoundingBox` creates a span which is a bounding box over two spans -}
spanBoundingBox :: Span (Vec n Int) -> Span (Vec n Int) -> Span (Vec n Int)
spanBoundingBox a b = boundingBox' (normaliseSpan a) (normaliseSpan b)
  where
    boundingBox' :: Span (Vec n Int) -> Span (Vec n Int) -> Span (Vec n Int)
    boundingBox' (Nil, Nil) (Nil, Nil)
        = (Nil, Nil)
    boundingBox' (Cons l1 ls1, Cons u1 us1) (Cons l2 ls2, Cons u2 us2)
        = let (ls', us') = boundingBox' (ls1, us1) (ls2, us2)
           in (Cons (min l1 l2) ls', Cons (max u1 u2) us')


{-| Given two spans, if they are consecutive
    (i.e., (lower1, upper1) (lower2, upper2) where lower2 = upper1 + 1)
    then compose together returning Just of the new span. Otherwise Nothing -}
composeConsecutiveSpans :: Span (Vec n Int)
                        -> Span (Vec n Int) -> Maybe (Span (Vec n Int))
composeConsecutiveSpans (Nil, Nil) (Nil, Nil) = Just (Nil, Nil)
composeConsecutiveSpans (Cons l1 ls1, Cons u1 us1) (Cons l2 ls2, Cons u2 us2)
    | (ls1 == ls2) && (us1 == us2) && (u1 + 1 == l2)
      = Just (Cons l1 ls1, Cons u2 us2)
    | otherwise
      = Nothing

foobar = [Cons 0 (Cons 1 Nil), Cons 1 (Cons 1 Nil)]

{-| |inferMinimalVectorRegions| a key part of the algorithm, from a list of
    n-dimensional relative indices it infers a list of (possibly overlapping)
    1-dimensional spans (vectors) within the n-dimensional space.
    Built from |minimalise| and |allRegionPermutations| -}
inferMinimalVectorRegions :: (Permutable n) => [Vec n Int] -> [Span (Vec n Int)]
inferMinimalVectorRegions = fixCoalesce . map mkTrivialSpan
  where fixCoalesce spans =
          let spans' = minimaliseRegions . allRegionPermutations $ spans
          in if spans' == spans then spans' else fixCoalesce spans'

{-| Map from a lists of n-dimensional spans of relative indices into all
    possible contiguous spans within the n-dimensional space (individual pass)-}
allRegionPermutations :: (Permutable n)
                      => [Span (Vec n Int)] -> [Span (Vec n Int)]
allRegionPermutations =
  nub . concat . unpermuteIndices . map (coalesceRegions >< id) . groupByPerm . map permutationss
    where
      {- Permutations of a indices in a span
         (independently permutes the lower and upper bounds in the same way) -}
      permutationss :: Permutable n
                   => Span (Vec n Int)
                   -> [(Span (Vec n Int), Vec n Int -> Vec n Int)]
      -- Since the permutation ordering is identical for lower & upper bound,
      -- reuse the same unpermutation
      permutationss (l, u) = map (\((l', un1), (u', un2)) -> ((l', u'), un1))
                           $ zip (permutationsV l) (permutationsV u)

      sortByFst        = sortBy (\(l1, u1) (l2, u2) -> compare l1 l2)

      groupByPerm  :: [[(Span (Vec n Int), Vec n Int -> Vec n Int)]]
                   -> [( [Span (Vec n Int)] , Vec n Int -> Vec n Int)]
      groupByPerm      = map (\ixP -> let unPerm = snd $ head ixP
                                      in (map fst ixP, unPerm)) . transpose

      coalesceRegions :: [Span (Vec n Int)] -> [Span (Vec n Int)]
      coalesceRegions  = nub . foldPair composeConsecutiveSpans . sortByFst

      unpermuteIndices :: [([Span (Vec n Int)], Vec n Int -> Vec n Int)]
                       -> [[Span (Vec n Int)]]
      unpermuteIndices = nub . map (\(rs, unPerm) -> map (unPerm *** unPerm) rs)

{-| Collapses the regions into a small set by looking for potential overlaps
    and eliminating those that overlap -}
minimaliseRegions :: [Span (Vec n Int)] -> [Span (Vec n Int)]
minimaliseRegions [] = []
minimaliseRegions xss = nub . minimalise $ xss
  where localMin x ys = (filter' x (\y -> containedWithin x y && (x /= y)) xss) ++ ys
        minimalise = foldr localMin []
        -- If nothing is caught by the filter, i.e. no overlaps then return
        -- the original regions r
        filter' r f xs = case filter f xs of
                           [] -> [r]
                           ys -> ys

{-| Binary predicate on whether the first region containedWithin the second -}
containedWithin :: Span (Vec n Int) -> Span (Vec n Int) -> Bool
containedWithin (Nil, Nil) (Nil, Nil)
  = True
containedWithin (Cons l1 ls1, Cons u1 us1) (Cons l2 ls2, Cons u2 us2)
  = (l2 <= l1 && u1 <= u2) && containedWithin (ls1, us1) (ls2, us2)


{-| Defines the (total) class of vector sizes which are permutable, along with
    the permutation function which pairs permutations with the 'unpermute'
    operation -}
class Permutable (n :: Nat) where
  -- From a Vector of length n to a list of 'selections'
  --   (triples of a selected element, the rest of the vector,
  --   a function to 'unselect')
  selectionsV :: Vec n a -> [Selection n a]
  -- From a Vector of length n to a list of its permutations paired with the
  -- 'unpermute' function
  permutationsV :: Vec n a -> [(Vec n a, Vec n a -> Vec n a)]

-- 'Split' is a size-indexed family which gives the type of selections
-- for each size:
--    Z is trivial
--    (S n) provides a triple of the select element, the remaining vector,
--           and the 'unselect' function for returning the original value
type family Selection n a where
            Selection Z a = a
            Selection (S n) a = (a, Vec n a, a -> Vec n a -> Vec (S n) a)

instance Permutable Z where
  selectionsV Nil   = []
  permutationsV Nil = [(Nil, id)]

instance Permutable (S Z) where
  selectionsV (Cons x xs)
    = [(x, Nil, Cons)]
  permutationsV (Cons x Nil)
    = [(Cons x Nil, id)]

instance Permutable (S n) => Permutable (S (S n)) where
  selectionsV (Cons x xs) =
    (x, xs, Cons) : [ (y, Cons x ys, unselect unSel)
                    | (y, ys, unSel) <- selectionsV xs ]
    where
     unselect :: (a -> Vec n a -> Vec (S n) a)
              -> (a -> Vec (S n) a -> Vec (S (S n)) a)
     unselect f y' (Cons x' ys') = Cons x' (f y' ys')

  permutationsV xs =
      [ (Cons y zs, \(Cons y' zs') -> unSel y' (unPerm zs'))
        | (y, ys, unSel) <- selectionsV xs,
          (zs,  unPerm)  <- permutationsV ys ]

{- Vector list repreentation where the size 'n' is existential quantified -}
data VecList a where VL :: Permutable n => [Vec n a] -> VecList a

-- Lists existentially quanitify over a vector's size : Exists n . Vec n a
data List a where
     List :: Permutable n => Vec n a -> List a

lnil :: List a
lnil = List Nil
lcons :: a -> List a -> List a
lcons x (List Nil) = List (Cons x Nil)
lcons x (List (Cons y Nil)) = List (Cons x (Cons y Nil))
lcons x (List (Cons y (Cons z xs))) = List (Cons x (Cons y (Cons z xs)))

fromList :: [a] -> List a
fromList = foldr lcons lnil

-- pre-condition: the input is a 'rectangular' list of lists (i.e. all internal
-- lists have the same size)
fromLists :: [[Int]] -> VecList Int
fromLists [] = VL ([] :: [Vec Z Int])
fromLists (xs:xss) = consList (fromList xs) (fromLists xss)
  where
    consList :: List Int -> VecList Int -> VecList Int
    consList (List vec) (VL [])     = VL [vec]
    consList (List vec) (VL (x:xs))
      = let (vec', x') = zipVec vec x
        in  -- Force the pre-condition equality
          case (preCondition x' xs, preCondition vec' xs) of
            (ReflEq, ReflEq) -> VL (vec' : (x' : xs))

            where -- At the moment the pre-condition is 'assumed', and therefore
              -- force used unsafeCoerce: TODO, rewrite
              preCondition :: Vec n a -> [Vec n1 a] -> EqT n n1
              preCondition xs x = unsafeCoerce ReflEq

-- Equality type
data EqT (a :: k) (b :: k) where
    ReflEq :: EqT a a

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End: