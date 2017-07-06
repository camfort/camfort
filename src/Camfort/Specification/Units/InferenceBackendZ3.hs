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
import Data.Maybe (maybeToList, fromJust, catMaybes, fromMaybe)
import Data.List ((\\), findIndex, partition, sortBy, nub, group, groupBy, intercalate, tails, sort)
import Data.Generics.Uniplate.Operations (rewrite, universeBi)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.ST
import Control.Arrow (first, second)
import qualified Data.Map.Strict as M
import qualified Data.Array as A
import Data.Ord (comparing)
import Data.Function (on)

import Camfort.Analysis.Annotations
import Camfort.Specification.Units.Environment

import Z3.Monad

import qualified Debug.Trace as D

type UnitZ3Map = M.Map (UnitInfo, UnitInfo) Symbol

unitsEval :: Z3 a -> IO a
unitsEval = evalZ3With (Just QF_NIA) (opt "unsat_core" True)

inferVariables :: Constraints -> IO (Result, Maybe [(VV, UnitInfo)])
inferVariables cons = unitsEval $ do
  (unitMap, ast) <- constraintsToZ3 cons
  -- forM_ (zip z3cons [1..]) $ \ (con, i) -> do
  --   -- l <- mkFreshBoolVar ("label " ++ show i)
  --   -- solverAssertAndTrack con l
  --   assert con
  astStr <- astToString ast
  D.traceM $ "AST = " ++ astStr
  assert ast
  withModel $ \ m -> do
    -- The unit map stores the mapping between each (lhsU, rhsU) and
    -- its corresponding Z3 Symbol. Therefore we sort and group each
    -- entry by its lhsU, and then check the solved integer value of
    -- the Z3 Symbol. That solved integer value corresponds to rhsU
    -- raised to that power. Take all of the rhsUs, raised to their
    -- respective powers, and combine them into a single UnitMul for
    -- each lhsU.
    let unitGroups = groupBy ((==) `on` (fst . fst)) . sortBy (comparing (fst . fst)) $ M.toList unitMap

    -- unitGroups =
    -- [[((lhs1, rhs1), pow1_1), ((lhs1, rhs2), pow1_2), ...], [((lhs2, rhs1), pow2_1), ((lhs2, rhs2), pow2_2), ...], ...]

    solvedUnits <- forM unitGroups $ \ unitGroup -> do
      unitList <- fmap catMaybes . forM unitGroup $ \ ((lhsU, rhsU), lhsSym) -> do
        lhsAST <- mkIntVar lhsSym
        x      <- fromMaybe (error "inferVariables missing piece of model") <$> evalInt m lhsAST
        if x == 0 then return Nothing
                  else return . Just $ UnitPow rhsU (fromIntegral x)
      let ((lhsU, _), _):_ = unitGroup
      let solvedUnit = simplifyUnits $ foldUnits unitList
      return (lhsU, solvedUnit)

    -- We are only interested in reporting the solutions to variables.
    let solvedVars = [ (vv, unit) | v@(UnitVar vv, unit) <- solvedUnits ]
    return solvedVars

-- returns the unitinfo and the generated unit-AST, integer-power-AST
rhsUnitInfoToZ3 :: UnitInfo -> Z3 (UnitInfo, (String, Integer))
rhsUnitInfoToZ3 (UnitPow u p) = do
  (u', (uName, _)) <- rhsUnitInfoToZ3 u
  return (u', (uName, floor p))
rhsUnitInfoToZ3 u = do
  return (u, (show u, 1))

lhsUnitInfoToZ3 :: String -> UnitInfo -> Z3 (UnitInfo, (Symbol, Integer))
lhsUnitInfoToZ3 rhsName (UnitPow u p) = do
  (u', (uSym, _)) <- lhsUnitInfoToZ3 rhsName u
  return (u', (uSym, floor p))
-- unitInfoToZ3 (UnitName n) = undefined -- FIXME: how to make this a 'RHS' var?
lhsUnitInfoToZ3 rhsName u = do
  s <- mkStringSymbol (show u ++ "_" ++ rhsName)
  return (u, (s, 1))

constraintsToZ3 :: Constraints -> Z3 (UnitZ3Map, AST)
constraintsToZ3 cons = do
  let shiftedCons = map shiftTerms $ flattenConstraints cons
  let allRHSes = nub $ concatMap snd shiftedCons

  -- for each constraint having a set of LHS terms and a set of RHS terms:
  mapEqs <- fmap concat . forM (map shiftTerms $ flattenConstraints cons) $ \ (lhs, rhs) -> do
    unitRhses <- mapM rhsUnitInfoToZ3 rhs

    -- for each RHS symbol and corresponding power build an equation of the form:
    --   lhs1_RHS * pow1 + lhs2_RHS * pow2 + ... + lhsN_RHS powN = pow_RHS
    fmap catMaybes . forM unitRhses $ \ (rhsU, (rhsName, rhsPow)) -> do
      unitLhses <- mapM (lhsUnitInfoToZ3 rhsName) lhs
      let m = M.fromList [ ((lhsU, rhsU), lhsSym) | (lhsU, (lhsSym, _)) <- unitLhses ]

      -- lhsTerms = [lhs1_RHS * pow1, lhs2_RHS * pow2, ..., lhsN_RHS powN]
      lhsTerms <- forM (map snd unitLhses) $ \ (lhsSym, lhsPow) -> do
        lhsSymAST <- mkIntVar lhsSym
        lhsPowAST <- mkInteger lhsPow
        mkMul [lhsSymAST, lhsPowAST]

      case lhsTerms of
        [] -> return Nothing
        _  -> do
          lhsSum    <- mkAdd lhsTerms
          rhsPowAST <- mkInteger rhsPow
          eq        <- mkEq lhsSum rhsPowAST
          return $ Just (m, eq)

  let m = M.unions (map fst mapEqs)
  let constraintsAST = map snd mapEqs

  -- build existential of the form:
  --   exists (lhs1_rhs1 Int) (lhs1_rhs2 Int) ... (lhs2_rhs1 Int) ... (lhsN_rhsM Int)

  let symbols = M.elems m
  intSort <- mkIntSort

  conjunctionAST <- mkAnd constraintsAST
  exists <- mkExists [] symbols (replicate (length symbols) intSort) conjunctionAST

  return (m, exists)

negateCons = map (\ (UnitPow u k) -> UnitPow u (-k))

-- Units that should appear on the right-hand-side of the matrix during solving
isUnitRHS (UnitPow (UnitName _) _)        = True
isUnitRHS (UnitPow (UnitParamEAPAbs _) _) = True
isUnitRHS _                               = False

-- | Shift UnitNames/EAPAbs poly units to the RHS, and all else to the LHS.
shiftTerms :: ([UnitInfo], [UnitInfo]) -> ([UnitInfo], [UnitInfo])
shiftTerms (lhs, rhs) = (lhsOk ++ negateCons rhsShift, rhsOk ++ negateCons lhsShift)
  where
    (lhsOk, lhsShift) = partition (not . isUnitRHS) lhs
    (rhsOk, rhsShift) = partition isUnitRHS rhs

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

foldUnits units
  | null units = UnitlessVar
  | otherwise  = foldl1 UnitMul units

approxEq a b = abs (b - a) < epsilon
epsilon = 0.001 -- arbitrary
