{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies, FlexibleInstances, InstanceSigs, FlexibleContexts #-}

module Analysis.StencilInferenceEngine where

import Data.List

import Helpers
import Helpers.Vec
import Test.HUnit

{- Intervals are triples of lower, "maybe" exact, upper.
   That is, if an exact value 'x' is known then 'Just x' is the middle
   part of the tuple. Otherwise 'Nothing' -}   
type Interval a = (a, Maybe a, a)

mkExact x = (x, Just x, x)
mkInexact x y = (x, Nothing, y)

{- Spans are a pair of a lower and upper bound -}
type Span a = (a, a)
mkTrivialSpan a = (a, a)

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


{-| Given two spans, if they are consecutive (i.e., (lower1, upper1) (lower2, upper2) where lower2 = upper1 + 1)
    then compose them together returning Just of the new span. Otherwise Nothing -}
composeConsecutiveSpans :: Span (Vec n Int) -> Span (Vec n Int) -> Maybe (Span (Vec n Int))
composeConsecutiveSpans (Nil, Nil) (Nil, Nil) = Just (Nil, Nil)
composeConsecutiveSpans x@(Cons l1 ls1, Cons u1 us1) y@(Cons l2 ls2, Cons u2 us2)
    | (ls1 == ls2) && (us1 == us2) && (ls1 == us2) && (u1 + 1 == l2) 
      = Just (Cons l1 ls1, Cons u2 us2)
    | otherwise
      = Nothing

{- Inference algorithm:
   Parameters: d (dimensionality
     for n in Fin(d) -}

{-| |inferMinimalVectorRegions| a key part of the algorithm, from a list of n-dimensional relative indices 
    it infers a list of (possibly overlapping) 1-dimensional spans (vectors) within the n-dimensional space.
    Built from |minimalise| and |allRegionPermutations| -}
inferMinimalVectorRegions :: (Permutable n) => [Vec n Int] -> [Span (Vec n Int)]
inferMinimalVectorRegions = minimaliseRegions . allRegionPermutations

{-| Map from a lists of n-dimensional relative indices into all possible contiguous 1-dimensional vectors
    within that n-dimensional space -}    
allRegionPermutations :: (Permutable n) => [Vec n Int] -> [[Span (Vec n Int)]]
allRegionPermutations ixs =
    unpermuteIndices . coalesceRegions . groupByPerm . permuteIndices $ ixs
  where
    permuteIndices :: Permutable n => [Vec n Int] -> [[(Vec n Int, Vec n Int -> Vec n Int)]]
    permuteIndices   = map permutationsV
      
    groupByPerm      = transpose
    
    mkRegions :: [Vec n Int] -> [Span (Vec n Int)]
    mkRegions = foldPair composeConsecutiveSpans . map mkTrivialSpan . sort 

    coalesceRegions :: [[(Vec n Int, Vec n Int -> Vec n Int)]]
                    -> [([Span (Vec n Int)], Vec n Int -> Vec n Int)]
    coalesceRegions  = map (\ixsP -> let unPerm = snd $ head ixsP
                                     in (mkRegions (map fst ixsP), unPerm))

    unpermuteIndices :: [([Span (Vec n Int)], Vec n Int -> Vec n Int)]
                     -> [[Span (Vec n Int)]]
    unpermuteIndices = map (\(rs, unPerm) -> map (\(l, u) -> (unPerm l, unPerm u)) rs)

{-| Collapses the regions into a small set by looking for potential overlaps and eliminating those
    that overlap -}
minimaliseRegions :: [[Span (Vec n Int)]] -> [Span (Vec n Int)] 
minimaliseRegions [] = []
minimaliseRegions xss = nub $ go xss'
  where xss' = concat xss
        go []     = []
        go (x:xs) = (filter' x (\y -> overlaps x y && (x /= y)) xss') ++ (go xs)
        -- If nothing is caught by the filter, i.e. no overlaps then return the original regions r
        filter' r f xs = case (filter f xs) of
                           [] -> [r]
                           ys -> ys
                           
{-| Binary predicate on whether the first region overlaps the second -}
overlaps :: Span (Vec n Int) -> Span (Vec n Int) -> Bool
overlaps (Nil, Nil) (Nil, Nil) 
  = True
overlaps (Cons l1 ls1, Cons u1 us1) (Cons l2 ls2, Cons u2 us2) 
  = if (l2 <= l1 && u1 <= u2)
    then overlaps (ls1, us1) (ls2, us2)
    else False

      
{-| Defines the (total) class of vector sizes which are permutable, along with the
     permutation function which pairs permutations with the 'unpermute' operation -}
class Permutable (n :: Nat) where
  selectionsV :: Vec n a -> [Selection n a]
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
  permutationsV Nil = []
  
instance Permutable (S Z) where
  selectionsV (Cons x xs)
    = [(x, Nil, Cons)]
  permutationsV (Cons x Nil)
    = [(Cons x Nil, id)]

instance Permutable (S n) => Permutable (S (S n)) where
  selectionsV (Cons x xs)
    = (x, xs, Cons) : [ (y, Cons x ys, unselect unSel) | (y, ys, unSel) <- selectionsV xs]
    where
      unselect :: (a -> Vec n a -> Vec (S n) a) -> (a -> Vec (S n) a -> Vec (S (S n)) a)
      unselect f y' (Cons x' ys') = Cons x' (f y' ys')

  permutationsV xs =
      [ (Cons y zs, \(Cons y' zs') -> (unSel y') (unPerm zs')) 
        | (y, ys, unSel) <- selectionsV xs,
          (zs,  unPerm)  <- permutationsV ys ]

-- *** Various properties of the code here

{- Properties of `spanBoundingBox`: idempotent and associative -}
prop_spanBoundingIdem :: Natural n -> Span (Vec n Int) -> Bool
prop_spanBoundingIdem w x
    = spanBoundingBox x x == (normaliseSpan x)

prop_spanBoundingAssoc :: Natural n -> Span (Vec n Int)
                                    -> Span (Vec n Int)
                                    -> Span (Vec n Int) -> Bool
prop_spanBoundingAssoc w x y z
    = spanBoundingBox x (spanBoundingBox y z)
   == spanBoundingBox (spanBoundingBox x y) z

{- Permutations that come with 'unpermute' functions are invertable -}
prop_perms_invertable :: (Permutable n) => Natural n -> Vec n Int -> Bool
prop_perms_invertable w xs
    = take (fact (lengthV xs)) (repeat xs)
    == map (\(xs, f) -> f xs) (permutationsV xs)

fact 0 = 1
fact n = n * (fact $ n - 1)

