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

{-

This files gives an executable implementation of the model for
abstract stencil specifications. This model is used to drive both
the specification checking and program synthesis features.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module Camfort.Specification.Stencils.Model ( Interval(..)
                                            , Bound(..)
                                            , approxVec
                                            , Offsets(..)
                                            , UnionNF
                                            , vecLength
                                            , unfCompare
                                            , optimise
                                            , maximas
                                            , Approximation(..)
                                            , lowerBound, upperBound
                                            , fromExact
                                            , Multiplicity(..)
                                            , Peelable(..)
                                            ) where

import qualified Control.Monad as CM

import           Algebra.Lattice
import           Data.Semigroup
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import           Data.Foldable
import           Data.SBV
import           Data.Data
import           Data.List (sortBy, nub)
import           Data.Maybe (fromJust)
import qualified Data.PartialOrd as PO

import qualified Camfort.Helpers.Vec as V
import System.IO.Unsafe

-- Utility container
class Container a where
  type MemberTyp a
  type CompTyp a

  member :: MemberTyp a -> a -> Bool
  compile :: a -> (CompTyp a -> SBool)

--------------------------------------------------------------------------------
-- Arbitrary sets representing offsets
--------------------------------------------------------------------------------

data Offsets =
    Offsets (S.Set Int64)
  | SetOfIntegers
  deriving Eq

instance Ord Offsets where
    Offsets s `compare` Offsets s' = s `compare` s'
    Offsets _ `compare` SetOfIntegers = LT
    SetOfIntegers `compare` Offsets _ = GT
    SetOfIntegers `compare` SetOfIntegers = EQ

instance Container Offsets where
  type MemberTyp Offsets = Int64
  type CompTyp Offsets = SInt64

  member i (Offsets s) = i `S.member` s
  member _ _ = True

  compile (Offsets s) i = i `sElem` map fromIntegral (S.toList s)
  compile SetOfIntegers _ = true

instance JoinSemiLattice Offsets where
  (Offsets s) \/ (Offsets s') = Offsets $ s `S.union` s'
  _ \/ _ = SetOfIntegers

instance MeetSemiLattice Offsets where
  (Offsets s) /\ (Offsets s') = Offsets $ s `S.intersection` s'
  off@Offsets{} /\ _ = off
  _ /\ o = o

instance Lattice Offsets

instance BoundedJoinSemiLattice Offsets where
  bottom = Offsets S.empty

instance BoundedMeetSemiLattice Offsets where
  top = SetOfIntegers

instance BoundedLattice Offsets

--------------------------------------------------------------------------------
-- Interval as defined in the paper
--------------------------------------------------------------------------------

data Bound = Arbitrary | Standard

-- | Interval data structure assumes the following:
-- 1. The first num. param. is less than the second;
-- 2. For holed intervals, first num. param. <= 0 <= second num. param.;
data Interval a where
  IntervArbitrary :: Int -> Int -> Interval Arbitrary
  IntervInfiniteArbitrary :: Interval Arbitrary
  IntervHoled     :: Int64 -> Int64 -> Bool -> Interval Standard
  IntervInfinite  :: Interval Standard

deriving instance Eq (Interval a)

instance Show (Interval Standard) where
  show IntervInfinite = "IntervInfinite"
  show (IntervHoled lb up p) =
    "Interv [" ++ show lb ++ "," ++ show up ++ "]^" ++ show p

approxInterv :: Interval Arbitrary -> Approximation (Interval Standard)
approxInterv (IntervArbitrary a b)
  | a > b = error
    "Interval condition violated: lower bound is bigger than the upper bound."
  | a <=  0, b >=  0 = Exact  $ IntervHoled a' b' True
  | a <= -1, b == -1 = Exact  $ IntervHoled a' 0  False
  | a ==  1, b >=  1 = Exact  $ IntervHoled 0  b' False
  | a >   1, b >   1 = Bound Nothing $ Just $ IntervHoled 0  b' False
  | a <  -1, b <  -1 = Bound Nothing $ Just $ IntervHoled a' 0  False
  | otherwise = error "Impossible: All posibilities are covered."
  where
    a' = fromIntegral a
    b' = fromIntegral b
approxInterv IntervInfiniteArbitrary = Exact IntervInfinite

approxVec :: forall n .
             V.Vec n (Interval Arbitrary)
          -> Approximation (V.Vec n (Interval Standard))
approxVec v =
  case findApproxIntervs stdVec of
    ([],_) -> Exact . fmap fromExact $ stdVec
    _      -> Bound Nothing (Just $ upperBound <$> stdVec)
  where
    stdVec :: V.Vec n (Approximation (Interval Standard))
    stdVec = fmap approxInterv v

    findApproxIntervs :: forall n . V.Vec n (Approximation (Interval Standard))
                      -> ([ Int ], [ Int ])
    findApproxIntervs v = findApproxIntervs' 0 v ([],[])

    findApproxIntervs' :: forall n . Int
                       -> V.Vec n (Approximation (Interval Standard))
                       -> ([ Int ], [ Int ])
                       -> ([ Int ], [ Int ])
    findApproxIntervs' _ V.Nil acc = acc
    findApproxIntervs' i (V.Cons x xs) (bixs, eixs) =
      findApproxIntervs' (i+1) xs $
        case x of
          Bound{} -> (i:bixs, eixs)
          Exact{} -> (bixs, i:eixs)

instance Container (Interval Standard) where
  type MemberTyp (Interval Standard) = Int64
  type CompTyp (Interval Standard) = SInt64

  member 0 (IntervHoled _ _ b) = b
  member i (IntervHoled a b _) = i >= a && i <= b
  member _ _ = True

  compile (IntervHoled i1 i2 b) i
    | b = inRange i range
    | otherwise = inRange i range &&& i ./= 0
    where
      range = (fromIntegral i1, fromIntegral i2)
  compile IntervInfinite _ = true

instance JoinSemiLattice (Interval Standard) where
  (IntervHoled lb ub noHole) \/ (IntervHoled lb' ub' noHole') =
    IntervHoled (min lb lb') (max ub ub') (noHole || noHole')
  _ \/ _ = top

instance MeetSemiLattice (Interval Standard) where
  (IntervHoled lb ub noHole) /\ (IntervHoled lb' ub' noHole') =
    IntervHoled (max lb lb') (min ub ub') (noHole && noHole')
  int@IntervHoled{} /\ _ = int
  _ /\ int = int

instance Lattice (Interval Standard)

instance BoundedJoinSemiLattice (Interval Standard) where
  bottom = IntervHoled 0 0 False

instance BoundedMeetSemiLattice (Interval Standard) where
  top = IntervInfinite

instance BoundedLattice (Interval Standard)

--------------------------------------------------------------------------------
-- Union of cartesian products normal form
--------------------------------------------------------------------------------

type UnionNF n a = NE.NonEmpty (V.Vec n a)

vecLength :: UnionNF n a -> V.Natural n
vecLength = V.lengthN . NE.head

instance Container a => Container (UnionNF n a) where
  type MemberTyp (UnionNF n a) = V.Vec n (MemberTyp a)
  type CompTyp (UnionNF n a) = V.Vec n (CompTyp a)
  member is = any (member' is)
    where
      member' is space = and $ V.zipWith member is space

  compile spaces is = foldr1 (|||) $ NE.map (`compile'` is) spaces
    where
      compile' space is =
        foldr' (\(set, i) -> (&&&) $ compile set i) true $ V.zip space is

instance JoinSemiLattice (UnionNF n a) where
  oi \/ oi' = oi <> oi'

instance BoundedLattice a => MeetSemiLattice (UnionNF n a) where
  (/\) = CM.liftM2 (V.zipWith (/\))

instance BoundedLattice a => Lattice (UnionNF n a)

unfCompare :: forall a b n . ( Container a,          Container b
                             , MemberTyp a ~ Int64,  MemberTyp b ~ Int64
                             , CompTyp a ~ SInt64,   CompTyp b ~ SInt64
                             )
           => UnionNF n a -> UnionNF n b -> Ordering
unfCompare oi oi' = unsafePerformIO $ do
    thmRes <- prove pred
    case thmRes of
      -- Tell the user if there was a hard proof error (e.g., if
      -- z3 is not installed/accessible).
      -- TODO: give more information
      ThmResult (ProofError _ msgs) -> fail $ unlines msgs
      _ ->
        if modelExists thmRes
        then do
          ce <- counterExample thmRes
          case V.fromList ce of
             V.VecBox cev ->
               case V.proveEqSize (NE.head oi) cev of
                 Just V.ReflEq ->
                   -- TODO: The second branch is defensive programming the
                   -- member check is not necessary unless the counter example
                   -- is bogus (it shouldn't be). Delete if it adversely
                   -- effects the performance.
                   if | cev `member` oi  -> return GT
                      | cev `member` oi' -> return LT
                      | otherwise -> fail
                         "Impossible: counter example is in \
                          \neither of the operands"
                 Nothing -> fail
                    "Impossible: Counter example size doesn't \
                    \match the original vector size."
        else return EQ
  where
    counterExample :: ThmResult -> IO [ Int64 ]
    counterExample thmRes =
      case getModelAssignment thmRes of
        Right (False, ce) -> return ce
        Right (True, _) -> fail "Returned probable model."
        Left str -> fail str

    pred :: Predicate
    pred = do
      freeVars <- (mkFreeVars . dimensionality) oi :: Symbolic [ SInt64 ]
      case V.fromList freeVars of
        V.VecBox freeVarVec ->
          case V.proveEqSize (NE.head oi) freeVarVec of
            Just V.ReflEq -> return $
              compile oi freeVarVec .== compile oi' freeVarVec
            Nothing -> fail $
              "Impossible: Free variables size doesn't match that of the " ++
              "union parameter."
    dimensionality = V.length . NE.head

--------------------------------------------------------------------------------
-- Optimise unions
--------------------------------------------------------------------------------

instance PO.PartialOrd Offsets where
  (Offsets s) <= (Offsets s') = s <= s'
  SetOfIntegers <= Offsets{} = False
  _ <= SetOfIntegers = True

instance PO.PartialOrd (Interval Standard) where
  (IntervHoled lb ub p) <= (IntervHoled lb' ub' p') =
    (p' || not p) && lb >= lb' && ub <= ub'
  IntervInfinite <= IntervHoled{} = False
  _ <= IntervInfinite = True

instance PO.PartialOrd a => PO.PartialOrd (V.Vec n a) where
  v <= v' = and $ V.zipWith (PO.<=) v v'

optimise :: UnionNF n (Interval Standard) -> UnionNF n (Interval Standard)
optimise = NE.fromList . maximas . fixedPointUnion . NE.toList
  where
    fixedPointUnion unf =
      let unf' = unionLemma . maximas $ unf
      in if unf' == unf then unf' else fixedPointUnion unf'

sensibleGroupBy :: Eq a =>
                   (a -> a -> Ordering)
                -> (a -> a -> Bool)
                -> [ a ]
                -> [ [ a ] ]
sensibleGroupBy ord p l = nub . map (\el -> sortBy ord . filter (p el) $ l) $ l

maximas :: [ V.Vec n (Interval Standard) ] -> [ V.Vec n (Interval Standard) ]
maximas = nub
        . fmap (head . PO.maxima)
        . sensibleGroupBy ord (PO.<=)
  where
    ord a b = fromJust $ a `PO.compare` b

-- | Union lemma says that if we have a product of intervals (as defined in
-- the paper) and we union two that agrees in each dimension except one.
-- The union is again a product of intervals that agrees with the original
-- dimensions in all dimensions except the original differing one. At that
-- point it is the union of intervals, which is itself still an interval.
unionLemma :: [ V.Vec n (Interval Standard) ] -> [ V.Vec n (Interval Standard) ]
unionLemma = map (foldr1 (V.zipWith (\/)))
           . sensibleGroupBy (\a b -> if a == b then EQ else LT) agreeButOne
  where
    -- This function returns true if two vectors agree at all points but one.
    -- It also holds if two vectors are identical.
    agreeButOne :: Eq a => V.Vec n a -> V.Vec n a -> Bool
    agreeButOne = go False
      where
        go :: Eq a => Bool -> V.Vec n a -> V.Vec n a -> Bool
        go _ V.Nil V.Nil = True
        go False (V.Cons x xs) (V.Cons y ys)
          | x == y = go False xs ys
          | otherwise = go True xs ys
        go True (V.Cons x xs) (V.Cons y ys)
          | x == y = go True xs ys
          | otherwise = False

--------------------------------------------------------------------------------
-- Injections for multiplicity and exactness
--------------------------------------------------------------------------------

data Approximation a = Exact a | Bound (Maybe a) (Maybe a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable)

fromExact :: Approximation a -> a
fromExact (Exact a) = a
fromExact _ = error "Can't retrieve from bounded as if it was exact."

lowerBound :: Approximation a -> a
lowerBound (Bound (Just a) _) = a
lowerBound (Bound Nothing _) = error "Approximation doesn't have a lower bound."
lowerBound (Exact a) = a

upperBound :: Approximation a -> a
upperBound (Bound _ (Just a)) = a
upperBound (Bound _ Nothing) = error "Approximation doesn't have a upper bound."
upperBound (Exact a) = a

class Peelable a where
  type CoreTyp a
  peel :: a -> CoreTyp a

data Multiplicity a = Mult a | Once a
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable)

instance Peelable (Multiplicity a) where
  type CoreTyp (Multiplicity a) = a

  peel (Mult a) = a
  peel (Once a) = a

{-
data Approximation a = Exact a | Lower a | Upper a
  deriving (Eq, Show, Functor, Data, Typeable)

instance Peelable Approximation where
  peel (Exact a) = a
  peel (Lower a) = a
  peel (Upper a) = a
-}
