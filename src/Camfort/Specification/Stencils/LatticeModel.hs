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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Camfort.Specification.Stencils.LatticeModel ( Interval(..)
                                                   , Offsets(..)
                                                   , UnionNF(..)
                                                   , ioCompare
                                                   , Approximation(..)
                                                   , Mult(..)
                                                   ) where

import qualified Control.Monad as CM

import           Algebra.Lattice
import           Data.Semigroup
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import           Data.Foldable
import           Data.SBV

import qualified Camfort.Helpers.Vec as V

-- Utility container
class Container a b c | a -> b, b -> c where
  member :: b -> a -> Bool
  compile :: a -> (c -> SBool)

--------------------------------------------------------------------------------
-- Arbitrary sets representing offsets
--------------------------------------------------------------------------------

data Offsets =
    Offsets (S.Set Integer)
  | SetOfIntegers
  deriving Eq

instance Container Offsets Integer SInteger where
  member i (Offsets s) = i `S.member` s
  member _ _ = True

  compile (Offsets s) i = i `sElem` map fromInteger (S.toList s)
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

data Interval =
    Interval Integer Integer Bool
  | InfiniteInterval
  deriving Eq

instance Container Interval Integer SInteger where
  member 0 (Interval _ _ b) = b
  member i (Interval a b _) = i >= a && i <= b
  member _ _ = True

  compile (Interval i1 i2 b) i
    | b = inRange i range
    | otherwise = inRange i range &&& i ./= 0
    where
      range = (fromInteger i1, fromInteger i2)
  compile InfiniteInterval _ = true

instance JoinSemiLattice Interval where
  (Interval lb ub noHole) \/ (Interval lb' ub' noHole') =
    Interval (min lb lb') (max ub ub') (noHole || noHole')
  _ \/ _ = InfiniteInterval

instance MeetSemiLattice Interval where
  (Interval lb ub noHole) /\ (Interval lb' ub' noHole') =
    Interval (max lb lb') (min ub ub') (noHole && noHole')
  int@Interval{} /\ _ = int
  _ /\ int = int

instance Lattice Interval

instance BoundedJoinSemiLattice Interval where
  bottom = Interval 0 0 False

instance BoundedMeetSemiLattice Interval where
  top = InfiniteInterval

instance BoundedLattice Interval

--------------------------------------------------------------------------------
-- Union of cartesian products normal form
--------------------------------------------------------------------------------

type UnionNF a = NE.NonEmpty [ a ]

instance (Container a Integer SInteger)
      => Container (UnionNF a) [ Integer ] [ SInteger ] where
  member is = any (member' is)
    where
      member' is space
        | length is == length space = and $ zipWith member is space
        | otherwise = error "Dimensionality of indices doesn't match the spec."

  compile spaces is = foldr1 (|||) $ NE.map (`compile'` is) spaces
    where
      compile' space is
        | length is == length space =
          foldr' (\(set, i) -> (&&&) $ compile set i) true $ zip space is

instance JoinSemiLattice (UnionNF a) where
  oi \/ oi' = oi <> oi'

instance BoundedLattice a => MeetSemiLattice (UnionNF a) where
  (/\) = CM.liftM2 (zipWith (/\))

instance BoundedLattice a => Lattice (UnionNF a)

ioCompare :: forall a b . ( Container a Integer SInteger
                          , Container b Integer SInteger )
          => UnionNF a -> UnionNF b -> IO Ordering
ioCompare oi oi' = do
    thmRes <- prove pred
    if modelExists thmRes
      then do
        ce <- counterExample thmRes
        -- TODO: The bit below is defensive programming the second member
        -- check should not be necessary unless the counter example is
        -- bogus (it shouldn't be). Delete if it adversely effects the
        -- performance.
        return $
          if ce `member` oi
            then GT
            else
              if ce `member` oi'
                then LT
                else error "Impossible: counter example is in neither of the oeprands"
      else return EQ
  where
    counterExample thmRes =
      case getModel thmRes of
        Right (False, ce) -> return ce
        Right (True, _) -> fail "Returned probable model."
        Left str -> fail str
    pred = do
      xs <- mkFreeVars $ dimensionality oi
      return $ compile oi xs .== compile oi' xs
    dimensionality = length . NE.head

--------------------------------------------------------------------------------
-- Injections for multiplicity and exactness
--------------------------------------------------------------------------------

data Approximation a = Exact a | Lower a | Upper a | Both a a deriving Functor
data Mult a = Mult a | Once a deriving Functor

peel :: Mult a -> a
peel (Mult a) = a
peel (Once a) = a
