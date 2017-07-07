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
{-# LANGUAGE LambdaCase #-}

module Camfort.Specification.Stencils.InferenceBackend
  ( coalesce
  , containedWithin
  , inferFromIndicesWithoutLinearity
  , inferMinimalVectorRegions
  , spansToApproxSpatial
  , Span
  ) where

import Data.List
import Data.Maybe
import Algebra.Lattice (joins1)

import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.DenotationalSemantics
import qualified Camfort.Helpers.Vec as V

import Camfort.Specification.Stencils.Syntax

{- Spans are a pair of a lower and upper bound -}

type Span a = (a, a)

spansToApproxSpatial :: [ Span (V.Vec (V.S n) Int) ]
                       -> Either String (Approximation Spatial)
spansToApproxSpatial spans = sequence . fmap intervalsToRegions $ approxUnion
  where
    approxVecs =
      toApprox . map (fmap absRepToInf . transposeVecInterval) $ spans
    approxUnion = fmap (optimise . joins1 . map return) approxVecs

    toApprox :: [ V.Vec n (Interval Arbitrary) ]
             -> Approximation [ V.Vec n (Interval Standard) ]
    toApprox vs
      | parts <- (elongatedPartitions . map approxVec) vs =
          case parts of
            (orgs, []) -> Exact . map fromExact $ orgs
            ([], elongs) -> Bound Nothing (Just $ map upperBound elongs)
            (orgs, elongs) -> Bound (Just . map upperBound $ orgs)
                                    (Just . map upperBound $ orgs ++ elongs)

    elongatedPartitions =
      partition $ \case { Exact{} -> True; Bound{} -> False }

    -- TODO: DELETE AS SOON AS POSSIBLE
    absRepToInf :: Interval Arbitrary -> Interval Arbitrary
    absRepToInf interv@(IntervArbitrary a b)
      | fromIntegral a == absoluteRep = IntervInfiniteArbitrary
      | fromIntegral b == absoluteRep = IntervInfiniteArbitrary
      | otherwise = interv
    absRepToInf interv = interv

    transposeVecInterval :: Span (V.Vec n Int) -> V.Vec n (Interval Arbitrary)
    transposeVecInterval (us, vs) = V.zipWith IntervArbitrary us vs

mkTrivialSpan :: V.Vec n Int -> Span (V.Vec n Int)
mkTrivialSpan V.Nil = (V.Nil, V.Nil)
mkTrivialSpan (V.Cons x xs) =
    if x == absoluteRep
    then (V.Cons (-absoluteRep) ys, V.Cons absoluteRep zs)
    else (V.Cons x ys, V.Cons x zs)
  where
    (ys, zs) = mkTrivialSpan xs

{-| From a list of vectors of integers, representing relative offsets,
    generate a specification (but does not do any linearity checking)
    (defaults to Mult). Instead let the front-end does
    the linearity check first as an optimimsation.
    Also defaults to the specification being for a stencil -}
inferFromIndicesWithoutLinearity :: V.VecList Int -> Specification
inferFromIndicesWithoutLinearity (V.VL ixs) =
    Specification (Mult . inferCore $ ixs) True

inferCore :: [V.Vec n Int] -> Approximation Spatial
inferCore subs =
    case V.proveNonEmpty . head $ subs of
      Just (V.ExistsEqT V.ReflEq) ->
        case spansToApproxSpatial . inferMinimalVectorRegions $ subs of
          Right a -> a
          Left msg -> error msg
      Nothing -> error "Input vectors are empty!"

{-| |inferMinimalVectorRegions| a key part of the algorithm, from a list of
    n-dimensional relative indices it infers a list of (possibly overlapping)
    1-dimensional spans (vectors) within the n-dimensional space.
    Built from |minimalise| and |allRegionPermutations| -}
inferMinimalVectorRegions :: [V.Vec n Int] -> [Span (V.Vec n Int)]
inferMinimalVectorRegions = fixCoalesce . map mkTrivialSpan
  where fixCoalesce spans =
          let spans' = minimaliseRegions . coalesceContiguous $ spans
          in if spans' == spans then spans' else fixCoalesce spans'

-- An alternative that is simpler and possibly quicker
coalesceContiguous :: [Span (V.Vec n Int)] -> [Span (V.Vec n Int)]
coalesceContiguous []  = []
coalesceContiguous [x] = [x]
coalesceContiguous [x, y] =
    case coalesce x y of
       Nothing -> [x, y]
       Just c  -> [c]
coalesceContiguous (x:xs) =
    case sequenceMaybes (map (coalesce x) xs) of
       Nothing -> x : coalesceContiguous xs
       Just cs -> coalesceContiguous (cs ++ xs)

sequenceMaybes :: Eq a => [Maybe a] -> Maybe [a]
sequenceMaybes xs | all (== Nothing) xs = Nothing
                  | otherwise = Just (catMaybes xs)

{-| Coalesce two intervals of vectors into one, if they are contiguous -}
coalesce :: Span (V.Vec n Int) -> Span (V.Vec n Int) -> Maybe (Span (V.Vec n Int))
coalesce (V.Nil, V.Nil) (V.Nil, V.Nil) = Just (V.Nil, V.Nil)
-- If two well-defined intervals are equal, then they cannot be coalesced
coalesce x y | x == y = Nothing
-- Otherwise
coalesce (V.Cons l1 ls1, V.Cons u1 us1) (V.Cons l2 ls2, V.Cons u2 us2)
  | l1 == l2 && u1 == u2
    = case coalesce (ls1, us1) (ls2, us2) of
        Just (l, u) -> Just (V.Cons l1 l, V.Cons u1 u)
        Nothing     -> Nothing
  | (u1 + 1 == l2) && (us1 == us2) && (ls1 == ls2)
    = Just (V.Cons l1 ls1, V.Cons u2 us2)
  | (u2 + 1 == l1) && (us1 == us2) && (ls1 == ls2)
    = Just (V.Cons l2 ls2, V.Cons u1 us1)
-- Fall through (also catches cases where the initial size pre-condition
-- has been violated in a use of `Helpers.Vec.fromLists`
coalesce _ _
    = Nothing

{-| Collapses the regions into a small set by looking for potential overlaps
    and eliminating those that overlap -}
minimaliseRegions :: [Span (V.Vec n Int)] -> [Span (V.Vec n Int)]
minimaliseRegions [] = []
minimaliseRegions xss = nub . minimalise $ xss
  where localMin x ys = filter' x (\y -> containedWithin x y && (x /= y)) xss ++ ys
        minimalise = foldr localMin []
        -- If nothing is caught by the filter, i.e. no overlaps then return
        -- the original regions r
        filter' r f xs = case filter f xs of
                           [] -> [r]
                           ys -> ys

{-| Binary predicate on whether the first region containedWithin the second -}
containedWithin :: Span (V.Vec n Int) -> Span (V.Vec n Int) -> Bool
containedWithin (V.Nil, V.Nil) (V.Nil, V.Nil)
  = True
containedWithin (V.Cons l1 ls1, V.Cons u1 us1) (V.Cons l2 ls2, V.Cons u2 us2)
  = (l2 <= l1 && u1 <= u2) && containedWithin (ls1, us1) (ls2, us2)
containedWithin _ _
  = False

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
