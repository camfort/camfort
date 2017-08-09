{-
   Copyright 2017, Matthew Danish, Dominic Orchard, Andrew Rice, Mistral Contrastin

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

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Camfort.Specification.Units.BackendTypes
  ( UnitSet, Dim, Sub, identDim, isIdentDim, dimFromUnitInfo, dimFromUnitInfos, dimToUnitInfo, dimToUnitInfos
  , subFromList, subToList, identSub, applySub, composeSubs, prop_composition, freeDimVars, dimSimplify
  , dimToConstraint, constraintToDim, dimMultiply, dimRaisePow, dimParamEq, dimFromList )
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Camfort.Specification.Units.Environment (UnitInfo(..), Constraint(..), flattenUnits, foldUnits, unitParamEq)
import Data.List (partition, foldl', foldl1', sortBy, maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

-- | Set of UnitInfos
type UnitSet = S.Set UnitInfo

-- | Represents a dimension: collection of units raised to a power,
-- multiplied together. Implemented as a map of unit -> power.
type Dim = M.Map UnitInfo Integer

-- | Represents a substitution: map of unit to be replaced ->
-- dimension to replace it with.
type Sub = M.Map UnitInfo Dim

-- Maintain invariant of Dim whereby all units raised to the zeroth
-- power are eliminated from the map.
removeZeroes :: Dim -> Dim
removeZeroes = M.filterWithKey f
  where
    f _ 0           = False
    f UnitlessVar _ = False
    f UnitlessLit _ = False
    f _ _           = True

-- Handy function to strip away the UnitPow & return the essence.
getUnitPow :: UnitInfo -> (UnitInfo, Integer)
getUnitPow (UnitPow u p) = (u', floor p * p')
  where (u', p') = getUnitPow u
getUnitPow u = (u, 1)

-- | The identity Dim: 1.
identDim :: Dim
identDim = M.empty

-- | Test for identity.
isIdentDim :: Dim -> Bool
isIdentDim = M.null . removeZeroes

-- | Convert from list of implicitly multipled units into a Dim.
dimFromUnitInfos :: [UnitInfo] -> Dim
dimFromUnitInfos = dimFromList . map getUnitPow

-- | Convert a UnitInfo to a Dim.
dimFromUnitInfo :: UnitInfo -> Dim
dimFromUnitInfo = dimFromUnitInfos . flattenUnits

-- | Convert a Dim into an implicitly multipled list of units.
dimToUnitInfos :: Dim -> [UnitInfo]
dimToUnitInfos = map (\ (u, p) -> UnitPow u (fromInteger p)) . M.toList

-- | Convert a Dim into a UnitInfo.
dimToUnitInfo :: Dim -> UnitInfo
dimToUnitInfo = foldUnits . dimToUnitInfos

-- | Convert a Constraint into a Dim where lhs/rhs = 1. Also normalise
-- the powers by dividing by the gcd and making the largest absolute
-- value power be positive.
constraintToDim :: Constraint -> Dim
constraintToDim (ConEq lhs rhs) = normalise (dimFromUnitInfo lhs `dimMultiply` dimRaisePow (-1) (dimFromUnitInfo rhs))
  where
    normalise dim
      | M.null dim = dim
      | otherwise  = M.map (`div` divisor) dim
      where
        divisor = (maxPow `div` abs(maxPow)) * gcds (M.elems dim)
        maxPow  = maximumBy (comparing abs) (M.elems dim)

    gcds []  = 1
    gcds [x] = x
    gcds xs  = foldl1' gcd xs

-- | Multiply two Dims
dimMultiply :: Dim -> Dim -> Dim
dimMultiply d1 d2 = removeZeroes $ M.unionWith (+) d1 d2

-- | Raise the dimension to the given power
dimRaisePow :: Integer -> Dim -> Dim
dimRaisePow 0 d = identDim
dimRaisePow k d = M.map (* k) d

-- | Compare two Dims, not minding the difference between
-- UnitParam*Abs and UnitParam*Use versions of the polymorphic
-- constructors. Varies from a 'constraint parametric equality'
-- operator because it doesn't assume that dimRaisePow can be used
-- arbitrarily. You may want a constraint parametric equality (TODO).
dimParamEq :: Dim -> Dim -> Bool
dimParamEq d1 d2 = dimParamEq' (M.toList d1) (M.toList d2)

dimParamEq' :: [(UnitInfo, Integer)] -> [(UnitInfo, Integer)] -> Bool
dimParamEq' [] []             = True
dimParamEq' [] _              = False
dimParamEq' ((u1, p1):d1') d2 = case partition (unitParamEq u1 . fst) d2 of
  ((u2, p2):d2', d2'') -> dimParamEq' (rem1 ++ d1') (rem2 ++ d2' ++ d2'')
    where
      (rem1, rem2) | p1 == p2 = ([], [])
                   | p1 < p2  = ([], [(u2, p2 - p1)])
                   | p1 > p2  = ([(u1, p1 - p2)], [])

  _                    -> False

-- | Create a constraint that the given Dim is equal to the identity unit.
dimToConstraint :: Dim -> Constraint
dimToConstraint = ConEq UnitlessLit . dimToUnitInfo

-- | Convert a list of units paired with their corresponding power into a Dim.
dimFromList :: [(UnitInfo, Integer)] -> Dim
dimFromList = removeZeroes . M.fromListWith (+)

-- | Convert a list of units paired with their corresponding
-- substitution (as a Dim) into a Sub. Note that this is equivalent to
-- repeatedly composing substitutions, so earlier substitutions will
-- affect later ones.
subFromList :: [(UnitInfo, Dim)] -> Sub
subFromList = foldl' composeSubs identSub . map (uncurry M.singleton)

-- | Convert a Sub into an association-list format of unit mapped to unit.
subToList :: Sub -> [(UnitInfo, UnitInfo)]
subToList = map (fmap dimToUnitInfo) . M.toList

-- | Identity substitution (empty).
identSub :: Sub
identSub = M.empty

-- | Identity substitution filled out with some identity entries for a
-- certain set of units (useful for when two substitutions have
-- different sets of keys).
identSubWith :: [UnitInfo] -> Sub
identSubWith = M.fromList . map (\ u -> (u, dimFromList [(u, 1)]))

-- | Apply a substitution to a dimension.
applySub :: Sub -> Dim -> Dim
applySub sub dim =
  removeZeroes $ M.unionsWith (+) [ M.map (*p) (M.singleton ui 1 `fromMaybe` M.lookup ui sub) | (ui, p) <- M.toList dim ]

-- | Compose two substitutions.
composeSubs :: Sub -> Sub -> Sub
composeSubs sub1 sub2 = M.map (applySub sub1) (M.unionWith (curry snd) ident1 sub2)
  where
    ident1 = identSubWith (M.keys sub1)

-- | Test the composition property: f (g x) == (f . g) x
prop_composition :: Dim -> Sub -> Sub -> Bool
prop_composition d s1 s2 = applySub s1 (applySub s2 d) == applySub (composeSubs s1 s2) d

-- | Extract a list of 'free dimension variables' from a given Dim.
freeDimVars :: Dim -> [UnitInfo]
freeDimVars = filter isVar . M.keys
  where
    isVar (UnitParamPosAbs {}) = True
    isVar (UnitParamPosUse {}) = True
    isVar (UnitParamVarAbs {}) = True
    isVar (UnitParamVarUse {}) = True
    isVar (UnitParamLitAbs {}) = True
    isVar (UnitParamLitUse {}) = True
    isVar (UnitLiteral {})     = True
    isVar (UnitVar {})         = True
    isVar _                    = False

-- | The 'dimSimplify' algorithm as shown in Kennedy's technical report Fig 3.4.
dimSimplify :: UnitSet -> Dim -> Sub
dimSimplify excludes dim
  | null valids = identSub

  | (u, x):_ <- valids, x < 0
  , sub1     <- M.singleton u (M.singleton u (-1))
  , sub2     <- dimSimplify excludes (applySub sub1 dim) = composeSubs sub2 sub1

  | (u, x):[] <- valids = M.singleton u (dimFromList ((u, 1):[(v, -div y x) | (v, y) <- invals]))

  | (u, x):_ <- valids
  , sub1     <- M.singleton u (dimFromList ((u, 1):[(v, -div y x) | (v, y) <- M.toList dim, v /= u]))
  , sub2     <- dimSimplify excludes (applySub sub1 dim) = composeSubs sub2 sub1

  where
    valids   = sortBy (comparing (abs . snd)) . filter ((`S.notMember` excludes) . fst) $ M.toList dim
    validSet = S.fromList (map fst valids)
    invals   = filter ((`S.notMember` validSet) . fst) $ M.toList dim

testVar x = UnitVar (x, x)

u0 = testVar "u0"
u1 = testVar "u1"
u2 = testVar "u2"
u3 = testVar "u3"
u4 = testVar "u4"

dim1 = dimFromList [(u1, 6), (u2, 15), (u3, -7), (u4, 12)]
dim2 = dimFromList [(u1, 2), (u2, 15), (u3, -9)]

test1 = applySub (dimSimplify (S.fromList [u3,u4]) dim1) dim1 == dimFromList [(u2, 3), (u3, 2)]

test2 =  (dimSimplify (S.fromList [u0]) (dimFromList [(u0, 1), (u1, -2)]))
