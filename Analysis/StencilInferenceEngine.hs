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

type Span a = (a, a)

upDimN :: Span (Vec n Int) -> Int -> Span (Vec (S n) Int)
upDimN (a, b) y = (Cons y a, Cons y b)

{-
norm :: [Vec n Int] -> [Span (Vec n) Int]
norm xs = sort xs
  where norm' []       = []
        norm' (x : xs) = (x, x) `comp
-}

--rowToSpans :: [Vec n Int] -> Vec n [Span (Vec n Int)]
--rowToSpans xs = sort xs 




{- composeRegions

   From a span of (n+1)-dimensional spans,
    if the inner span describe two regions of the same size, shape, and position in (n)-dimension
    and in the (n+1)-dimension these are two apart, then coalesce into a single (n+1) span.

-}

composeRegions :: Span (Vec (S n) Int) -> Span (Vec (S n) Int) -> Interval (Span (Vec (S n) Int))
composeRegions x y = composeRegions' (normaliseSpan x) (normaliseSpan y)
  where composeRegions' :: Span (Vec (S n) Int) -> Span (Vec (S n) Int) -> Interval (Span (Vec (S n) Int))
        composeRegions' x@(Cons l1 ls1, Cons u1 us1) y@(Cons l2 ls2, Cons u2 us2)
            | (ls1 == ls2) && (us1 == us2) && (l2 == l1 + 1) && (u1 == l1) && (u2 == l2)
              = mkExact (Cons l1 ls1, Cons u2 us2)
   
            | otherwise
              = mkInexact undefined (spanBoundingBox x y)

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



{-

  | 1 2 3 4 5 6
----------------
1 | 0 0 1 1 1 0 
2 | 0 0 1 1 1 0 

-> 

3-5 in y = 1
(3,1) - (5,1)

3-5 in 7 = 2
(3,2) - (5,2)

vs say

(1, 3)  (2, 3) 
(1, 5)  (2, 5)

-}

{-
createSpan2D :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
createSpan2D (x, y) (a, b) =
-}


{-

0 1 0 
1 1 1
0 1 0
in two dimensions

i->j
[((0, -1), (0, -1)),    ((-1, 0), (1, 0)),    ((0, 1), (0, 1))]

j->i
[((-1, 0), (-1, 0)),    ((0, -1), (0, 1)),    ((1, 0), (1, 0))]

how can we find them equivalent? and even compose them into:

[((0, -1), (0, 1)),   ((-1, 0), (1, 0))]

0 0 0   0 1 0    0 0 0 
0 1 0   1 1 1    0 1 0
0 0 0   0 1 0    0 0 0
in three dimensions
i -> j -> k
[((0, 0, -1), (0, 0, -1)),   ((0, -1, 0), (0, -1, 0)),    ((-1, 0, 0), (1, 0, 0)),    ((0, 1, 0), (0, 1, 0))
 ((0, 0,  1), (0, 0,  1))]

k -> j -> i
[((-1, 0, 0), (-1, 0, 0)),   ((0, -1, 0), (0, -1, 0)),    ((0, 0, -1), (0, 0, 1)),    ((0, 1, 0), (0, 1, 0)),  
  ((1, 0, 0), (1, 0, 0))]

a third way....
output
[((0, 0, -1), (0, 0, 1)), ((0, -1, 0), (0, 1, 0)), ((-1, 0, 0), (1, 0, 0))]
-}

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

trivialSpan a = (a, a)
isTrivialSpan (x, y) = x == y

allRegionPermutations :: (Permutable n) => [Vec n Int] -> [[Span (Vec n Int)]]
allRegionPermutations ixs =
    unpermuteIndices . coalesceRegions . groupByPerm . permuteIndices $ ixs
  where
    permuteIndices :: Permutable n => [Vec n Int] -> [[(Vec n Int, Vec n Int -> Vec n Int)]]
    permuteIndices   = map permutationsV
      
    groupByPerm      = transpose
    
    mkRegions :: [Vec n Int] -> [Span (Vec n Int)]
    mkRegions = foldPair composeConsecutiveSpans . map trivialSpan . sort 

    coalesceRegions :: [[(Vec n Int, Vec n Int -> Vec n Int)]]
                    -> [([Span (Vec n Int)], Vec n Int -> Vec n Int)]
    coalesceRegions  = map (\ixsP -> let unPerm = snd $ head ixsP
                                     in (mkRegions (map fst ixsP), unPerm))

    unpermuteIndices :: [([Span (Vec n Int)], Vec n Int -> Vec n Int)]
                     -> [[Span (Vec n Int)]]
    unpermuteIndices = map (\(rs, unPerm) -> map (\(l, u) -> (unPerm l, unPerm u)) rs)

minimalise :: [[Span (Vec n Int)]] -> [Span (Vec n Int)] 
minimalise [] = []
minimalise xss = nub $ go xss'
  where xss' = concat xss
        go []     = []
        go (x:xs) = (filter' x (\y -> overlaps x y && (x /= y)) xss') ++ (go xs)

        filter' y f xs = case (filter f xs) of
                              [] -> [y]
                              ys -> ys

overlaps :: Span (Vec n Int) -> Span (Vec n Int) -> Bool
overlaps (Nil, Nil) (Nil, Nil) 
  = True
overlaps (Cons l1 ls1, Cons u1 us1) (Cons l2 ls2, Cons u2 us2) 
  = if (l2 <= l1 && u1 <= u2)
    then overlaps (ls1, us1) (ls2, us2)
    else False

      
       
{-| Defines the (total) class of vector sizes which are pemutable -}
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

