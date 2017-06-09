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
  Units of measure extension to Fortran: backend (Z3)
-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camfort.Specification.Units.InferenceBackendZ3
  -- ( inconsistentConstraints, criticalVariables, inferVariables
  -- -- mainly for debugging and testing:
  -- , shiftTerms, flattenConstraints, flattenUnits, constraintsToMatrix, constraintsToMatrices
  -- , rref, isInconsistentRREF, genUnitAssignments )
where

import Data.Tuple (swap)
import Data.Maybe (maybeToList, fromJust, catMaybes)
import Data.List ((\\), findIndex, partition, sortBy, group, intercalate, tails, sort)
import Data.Generics.Uniplate.Operations (rewrite, universeBi)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.ST
import Control.Arrow (first, second)
import qualified Data.Map.Strict as M
import qualified Data.Array as A

import Camfort.Analysis.Annotations
import Camfort.Specification.Units.Environment

import Z3.Monad

import qualified Debug.Trace as D

unitsEval :: Z3 a -> IO a
unitsEval = evalZ3With (Just QF_NIA) (opt "unsat_core" True)

inferVariables :: Constraints -> IO (Result, Maybe [(UnitInfo, Integer)])
inferVariables cons = unitsEval $ do
  (unitMap, z3cons) <- constraintsToZ3 cons
  forM_ (zip z3cons [1..]) $ \ (con, i) -> do
    -- l <- mkFreshBoolVar ("label " ++ show i)
    -- solverAssertAndTrack con l
    assert con
  withModel $ \ m -> do
    let vars = [ v | v@(UnitVar _, _) <- M.toList unitMap ]
    fmap catMaybes . forM vars $ \ (u, uAST) -> do
      fmap (u,) <$> evalInt m uAST

unitInfoToZ3 :: UnitInfo -> Z3 (UnitInfo, AST)
unitInfoToZ3 (UnitPow u p) = do
  (u', uAST) <- unitInfoToZ3 u
  pAST       <- mkInteger (floor p)
  m          <- mkMul [uAST, pAST]
  return (u', m)
-- unitInfoToZ3 (UnitName n) = undefined -- FIXME: how to make this a 'RHS' var?
unitInfoToZ3 u = do
  v <- mkFreshIntVar (show u)
  return (u, v)

constraintsToZ3 :: Constraints -> Z3 (M.Map UnitInfo AST, [AST])
constraintsToZ3 cons = do
  mapEqs <- forM (flattenConstraints cons) $ \ (lhs, rhs) -> do
    unitLhses <- mapM unitInfoToZ3 lhs
    unitRhses <- mapM unitInfoToZ3 rhs
    let m = M.fromList (unitLhses ++ unitRhses)
    lhsAdd <- mkAdd (map snd unitLhses)
    rhsAdd <- mkAdd (map snd unitRhses)
    eq <- mkEq lhsAdd rhsAdd
    return (m, eq)
  let m = M.unions (map fst mapEqs)
  let asts = map snd mapEqs
  return (m, asts)

-- | Translate all constraints into a LHS, RHS side of units.
flattenConstraints :: Constraints -> [([UnitInfo], [UnitInfo])]
flattenConstraints = map (\ (ConEq u1 u2) -> (flattenUnits u1, flattenUnits u2))

simplifyUnits :: UnitInfo -> UnitInfo
simplifyUnits = rewrite rw
  where
    rw (UnitMul (UnitMul u1 u2) u3)                          = Just $ UnitMul u1 (UnitMul u2 u3)
    rw (UnitMul u1 u2) | u1 == u2                            = Just $ UnitPow u1 2
    rw (UnitPow (UnitPow u1 p1) p2)                          = Just $ UnitPow u1 (p1 * p2)
    rw (UnitMul (UnitPow u1 p1) (UnitPow u2 p2)) | u1 == u2  = Just $ UnitPow u1 (p1 + p2)
    rw (UnitPow _ p) | p `approxEq` 0                        = Just UnitlessLit
    rw (UnitMul UnitlessLit u)                               = Just u
    rw (UnitMul u UnitlessLit)                               = Just u
    rw u                                                     = Nothing

flattenUnits :: UnitInfo -> [UnitInfo]
flattenUnits = map (uncurry UnitPow) . M.toList
             . M.filterWithKey (\ u _ -> u /= UnitlessLit)
             . M.filter (not . approxEq 0)
             . M.fromListWith (+)
             . map (first simplifyUnits)
             . flatten
  where
    flatten (UnitMul u1 u2) = flatten u1 ++ flatten u2
    flatten (UnitPow u p)   = map (second (p*)) $ flatten u
    flatten u               = [(u, 1)]

approxEq a b = abs (b - a) < epsilon
epsilon = 0.001 -- arbitrary
