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
  Units of measure extension to Fortran: backend
      -- declare-const -> exists / mkExistVars
      -- define-fun -> namedConstraint
-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Camfort.Specification.Units.InferenceBackendSBV
  -- ( inconsistentConstraints, criticalVariables, inferVariables
  -- -- mainly for debugging and testing:
  -- , shiftTerms, flattenConstraints, flattenUnits, constraintsToMatrix, constraintsToMatrices
  -- , rref, isInconsistentRREF, genUnitAssignments )
where

import Data.Char
import Data.Tuple (swap)
import Data.Maybe (maybeToList, catMaybes, fromMaybe)
import Data.List ((\\), isPrefixOf, findIndex, partition, sortBy, group, groupBy, tails, transpose, nub, intercalate)
import Data.Generics.Uniplate.Operations (rewrite)
import Debug.Trace (trace, traceShowM, traceM)
import Control.Monad
import Control.Monad.ST
import Control.Arrow (first, second)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Array as A
import System.IO.Unsafe (unsafePerformIO)
import Data.SBV -- ( transcript, SatResult(..), SMTResult(Unknown), Symbolic, SBool, SInteger, SBV
                -- , satWith, z3, getModelDictionary, fromCW, true, namedConstraint, (.==)
                -- , sInteger, literal, bAnd, Predicate, getModelValue, setOption )
import Data.SBV.Control
import Data.Ord (comparing)
import Data.Function (on)

import Camfort.Specification.Units.Environment

import Numeric.LinearAlgebra (
    atIndex, (<>), rank, (?), rows, cols,
    takeColumns, dropRows, subMatrix, diag, fromBlocks,
    ident,
  )
import qualified Numeric.LinearAlgebra as H
import Numeric.LinearAlgebra.Devel (
    newMatrix, readMatrix, writeMatrix, runSTMatrix, freezeMatrix, STMatrix
  )


--------------------------------------------------

-- | Identifies the variables that need to be annotated in order for
-- inference or checking to work.
criticalVariables :: Constraints -> [UnitInfo]
criticalVariables _ = [] -- STUB

--------------------------------------------------

simplifyUnits :: UnitInfo -> UnitInfo
simplifyUnits = rewrite rw
  where
    rw (UnitMul (UnitMul u1 u2) u3)                          = Just $ UnitMul u1 (UnitMul u2 u3)
    rw (UnitMul u1 u2) | u1 == u2                            = Just $ UnitPow u1 2
    rw (UnitPow (UnitPow u1 p1) p2)                          = Just $ UnitPow u1 (p1 * p2)
    rw (UnitMul (UnitPow u1 p1) (UnitPow u2 p2)) | u1 == u2  = Just $ UnitPow u1 (p1 + p2)
    rw (UnitPow UnitlessLit _)                               = Just UnitlessLit
    rw (UnitPow UnitlessVar _)                               = Just UnitlessVar
    rw (UnitPow _ p) | p `approxEq` 0                        = Just UnitlessLit
    rw (UnitMul UnitlessLit u)                               = Just u
    rw (UnitMul u UnitlessLit)                               = Just u
    rw (UnitMul UnitlessVar u)                               = Just u
    rw (UnitMul u UnitlessVar)                               = Just u
    rw _                                                     = Nothing

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

--------------------------------------------------

negateCons = map (\ (UnitPow u k) -> UnitPow u (-k))

negatePosAbs (UnitPow (UnitParamPosAbs x) k) = UnitPow (UnitParamPosAbs x) (-k)
negatePosAbs u                               = u

--------------------------------------------------

-- Units that should appear on the right-hand-side of the equations during solving
isUnitRHS1, isUnitRHS2 :: UnitInfo -> Bool
isUnitRHS1 (UnitPow (UnitName _) _)        = True
isUnitRHS1 (UnitPow (UnitParamEAPAbs _) _) = True
isUnitRHS1 _                               = False

-- during interpretation
isUnitRHS2 (UnitPow (UnitName _) _)        = True
isUnitRHS2 (UnitPow (UnitParamEAPAbs _) _) = True
isUnitRHS2 (UnitPow (UnitParamPosAbs _) _) = True
isUnitRHS2 _                               = False

-- | Shift UnitNames/EAPAbs poly units to the RHS, and all else to the LHS.
shiftTerms :: (UnitInfo -> Bool) -> ([UnitInfo], [UnitInfo]) -> ([UnitInfo], [UnitInfo])
shiftTerms isUnitRHS (lhs, rhs) = (lhsOk ++ negateCons rhsShift, rhsOk ++ negateCons lhsShift)
  where
    (lhsOk, lhsShift)           = partition (not . isUnitRHS) lhs
    (rhsOk, rhsShift)           = partition isUnitRHS rhs

-- | Translate all constraints into a LHS, RHS side of units.
flattenConstraints :: Constraints         -> [([UnitInfo], [UnitInfo])]
flattenConstraints = map (\ (ConEq u1 u2) -> (flattenUnits u1, flattenUnits u2))

foldUnits units
  | null units = UnitlessVar
  | otherwise  = foldl1 UnitMul units

--------------------------------------------------

type Z3 a   = Symbolic a
type Symbol = SInteger

type UnitZ3Map = M.Map (UnitInfo, UnitInfo) Symbol

type LhsUnit         = UnitInfo
type RhsUnit         = UnitInfo
type UnitInfoNameMap = M.Map String (LhsUnit, RhsUnit)
type NameSIntegerMap = M.Map String SInteger

gatherRhsUnitInfoNames :: [[UnitInfo]] -> [(String, RhsUnit)]
gatherRhsUnitInfoNames = concatMap eachRow
  where
    eachRow            = map eachCol

    eachCol (UnitPow u _) = (show u, u)
    eachCol u             = (show u, u)

gatherLhsUnitInfoNames :: (String, RhsUnit) -> [[UnitInfo]] -> [(String, (LhsUnit, RhsUnit))]
gatherLhsUnitInfoNames (rhsName, rhsUnit) = concatMap eachRow
  where
    eachRow                               = map eachCol

    eachCol (UnitPow u _) = (show u ++ "_" ++ rhsName, (u, rhsUnit))
    eachCol u             = (show u ++ "_" ++ rhsName, (u, rhsUnit))

gatherUnitInfoNameMap :: [([UnitInfo], [UnitInfo])] -> UnitInfoNameMap
gatherUnitInfoNameMap shiftedCons = M.fromListWith (curry fst) lhsNames
  where
    lhsNames                      = concatMap (flip gatherLhsUnitInfoNames lhsRows) rhsNames
    lhsRows                       = map fst shiftedCons

    rhsNames = gatherRhsUnitInfoNames rhsRows
    rhsRows = map snd shiftedCons

genUnitAssignmentsSBV :: Constraints -> [(UnitInfo, UnitInfo)]
genUnitAssignmentsSBV cons = unsafePerformIO $ do
  let shiftedCons :: [([UnitInfo], [UnitInfo])]
      shiftedCons = map (shiftTerms isUnitRHS1) $ flattenConstraints cons
  traceShowM cons
  traceShowM shiftedCons
  let uiNameMap = gatherUnitInfoNameMap shiftedCons

  let genVar :: String -> Symbolic (String, SInteger)
      genVar name = (name,) <$> sInteger name

  let rhsNames :: [(String, UnitInfo)]
      rhsNames = gatherRhsUnitInfoNames (map snd shiftedCons)

  -- start off with every RHS mapped to a power of zero.
  let baseRhsMap = M.fromList [ (name, 0) | (name, _) <- rhsNames ]

  let pred :: Symbolic (Either [String] [(UnitInfo, UnitInfo)])
      pred = do
        setOption $ ProduceUnsatCores True
        -- pregenerate all of the necessary existentials
        uiSymMap <- M.fromList <$> mapM genVar (M.keys uiNameMap)

        let getRhsDetails :: UnitInfo -> (String, Integer)
            getRhsDetails (UnitPow u p) = (uName, floor p * p')
              where (uName, p') = getRhsDetails u
            getRhsDetails u = (show u, 1)

        let getLhsSymbol :: String -> UnitInfo -> (Symbol, Integer)
            getLhsSymbol rhsName (UnitPow u p) = (uSym, floor p * p')
              where (uSym, p') = getLhsSymbol rhsName u
            getLhsSymbol rhsName u = (s, 1)
              where n = show u ++ "_" ++ rhsName
                    s = error ("missing variable for " ++ n) `fromMaybe` M.lookup n uiSymMap

        -- for each RHS name and corresponding power build an equation of the form:
        --   lhs1_RHS * pow1 + lhs2_RHS * pow2 + ... + lhsN_RHS powN = pow_RHS
        let eachRhs :: [UnitInfo] -> (String, Integer) -> Maybe (SBool, String)
            eachRhs lhs (rhsName, rhsPow)
              | null lhsTerms = Nothing
              | otherwise     = Just (sum lhsTerms .== literal rhsPow, msg)
              where
                -- lhsTerms = [lhs1_RHS * pow1, lhs2_RHS * pow2, ..., lhsN_RHS powN]
                lhsTerms :: [SInteger]
                lhsTerms = [ lhsSym * literal lhsPow | lhs_i <- lhs
                                                     , let (lhsSym, lhsPow) = getLhsSymbol rhsName lhs_i ]
                msg = intercalate " + " [ lhsName ++ "(" ++ rhsName ++ ") * " ++ show lhsPow
                                        | lhs_i <- lhs
                                        , let (lhsName, lhsPow) = getRhsDetails lhs_i ] ++
                      " == " ++ rhsName ++ " * " ++ show rhsPow

        -- for each constraint having a set of LHS terms and a set of RHS terms:
        let eachConstraint :: ([UnitInfo], [UnitInfo]) -> [(SBool, String)]
            eachConstraint (lhs, rhs) = res
              where
                msg       = "eachConstraint " ++ show (lhs, rhs) ++ " = " ++ show res
                res       = catMaybes . map (eachRhs lhs) $ rhsPowers
                -- map every RHS to its corresponding power (including 0 for those not mentioned)
                rhsPowers = M.toList . M.unionWith (+) baseRhsMap . M.fromListWith (+) . map getRhsDetails $ rhs
        -- conjunction of all constraints
        let sbools = concatMap eachConstraint shiftedCons
        forM_ (zip [1..] sbools) $ \ (i, (sbool, msg)) -> do
          traceM $ "c"++show i++": " ++ msg
          namedConstraint ("c"++show i) sbool

        query $ do
          cs <- checkSat
          case cs of
            Unsat -> Left <$> getUnsatCore
            Sat -> do
              nvMap <- extractSIntValues uiSymMap
              disallowValues uiSymMap nvMap
              cs <- checkSat
              case cs of
                Sat -> do
                  io $ putStrLn "Yes there are multiple solutions"
                  nvMap' <- extractSIntValues uiSymMap
                  let m = M.unionWith (S.union) (M.map S.singleton nvMap) (M.map S.singleton nvMap')
                  io $ print m
                _   -> io $ putStrLn "No bupkis"

              Right <$> interpret uiNameMap nvMap
            -- try to compute multiple satisfiable assignments
            -- if there is more than 1 that means either poly/units-suggest
            _ -> error "unknown"




  satResult <- runSMTWith z3 { transcript = Just "mrd.smt2" } -- SMT-LIB dump
               pred

  case satResult of
    -- SatResult (Unknown _ msg) -> do
    --   putStrLn $ "Solver returned unknown error: " ++ msg
    --   return []
    -- SatResult (ProofError _ msgs) -> do
    --   putStrLn $ "Solver errored: " ++ intercalate ", " msgs
    --   return []
    -- SatResult (Unsatisfiable _) -> do
    --   -- core <- getUnsatCore
    --   -- putStrLn $ "Unsatisfiable: " ++ show core
    --   return []
    -- SatResult (Satisfiable _ _) -> do
    Left core -> do
      putStrLn $ "Unsatisfiable: " ++ show core
      return []
    Right assigns -> return assigns

      -- -- FIXME: refactor so it doesn't run twice
      -- allSatRes <- allSatWith z3 { transcript = Just "mrd.smt2" } -- SMT-LIB dump
      --              pred
      -- let AllSatResult (maxModelLimit, prefixExis, smtResults) = allSatRes
      -- traceM $ "maxModelLimit = " ++ show maxModelLimit ++ ", prefixExis = " ++ show prefixExis
      -- fmap concat . forM smtResults $ \ smtResult -> do
      --   let satResult = SatResult smtResult
      --   traceShowM satResult
      --   let lhsU = fst . snd
      --   let unitGroups = groupBy ((==) `on` lhsU) . sortBy (comparing lhsU) $ M.toList uiNameMap

      --   -- unitGroups =
      --   --   [ [(name1_1, (lhs1, rhs1)), (name1_2, (lhs1, rhs2)), ...]
      --   --   , [(name2_1, (lhs2, rhs1)), (name2_2, (lhs2, rhs2)), ...]
      --   --   , ...]

      --   -- each name corresponds to an LHS/RHS combo encoded into the solver
      --   let eachName :: (String, (LhsUnit, RhsUnit)) -> Maybe UnitInfo
      --       eachName (lhsName, (lhsU, rhsU))
      --         | x == 0    = Nothing
      --         | otherwise = Just $ UnitPow rhsU (fromInteger x)
      --         where
      --           x = error "inferVariables missing piece of model" `fromMaybe` getModelValue lhsName satResult

      --   -- each group corresponds to a LHS variable
      --   let eachGroup :: [(String, (LhsUnit, RhsUnit))] -> (LhsUnit, UnitInfo)
      --       eachGroup unitGroup = (lhsU, solvedUnit)
      --         where
      --           (_, (lhsU, _)):_ = unitGroup -- grouped by lhsU, so pick out one of them
      --           solvedUnit = simplifyUnits . foldUnits . catMaybes $ map eachName unitGroup

      --   return $ map eachGroup unitGroups

data ValueInfo
  = VISet [Integer]
  | VISuggest
  | VIParametric Integer
  deriving (Show, Eq, Ord)

type NameValueInfoMap = M.Map String ValueInfo

type NameValueMap = M.Map String Integer

extractSIntValues :: NameSIntegerMap -> Query NameValueMap
extractSIntValues = (M.fromList <$>) . mapM (\ (name, sInt) -> (name,) <$> getValue sInt) . M.toList

disallowValues :: NameSIntegerMap -> NameValueMap -> Query ()
disallowValues uiSIntMap nvMap = constrain . bOr . catMaybes $ map mkNotEq (M.toList nvMap)
  where mkNotEq (name, value) = (./= literal value) <$> M.lookup name uiSIntMap

disallowCurrentValues :: NameSIntegerMap -> Query ()
disallowCurrentValues uiSIntMap = extractSIntValues uiSIntMap >>= disallowValues uiSIntMap

-- Interpret results.

-- The uiNameMap stores the mapping between each SInteger name and
-- its corresponding (lhsU, rhsU). Therefore we sort and group each
-- entry by its lhsU, and then check the solved integer value of the
-- SInteger name. That solved integer value corresponds to rhsU raised
-- to that power. Take all of the rhsUs, raised to their respective
-- powers, and combine them into a single UnitMul for each lhsU.

interpret :: UnitInfoNameMap -> NameValueMap -> Query [(UnitInfo, UnitInfo)]
interpret uiNameMap nvMap = do
  let lhsU = fst . snd
  let unitGroups = groupBy ((==) `on` lhsU) . sortBy (comparing lhsU) $ M.toList uiNameMap

  -- unitGroups =
  --   [ [(name1_1, (lhs1, rhs1)), (name1_2, (lhs1, rhs2)), ...]
  --   , [(name2_1, (lhs2, rhs1)), (name2_2, (lhs2, rhs2)), ...]
  --   , ...]

  let eachName :: (String, (LhsUnit, RhsUnit)) -> Query (Maybe UnitInfo)
      eachName (lhsName, (lhsU, rhsU)) = do
        case M.lookup lhsName nvMap of
          Nothing -> return Nothing
          Just 0  -> return Nothing
          Just x  -> return . Just $ UnitPow rhsU (fromInteger x)

  -- each group corresponds to a LHS variable
  let eachGroup :: [(String, (LhsUnit, RhsUnit))] -> Query (LhsUnit, UnitInfo)
      eachGroup unitGroup = do
        let (_, (lhsU, _)):_ = unitGroup -- grouped by lhsU, so pick out one of them
        solvedUnit <- (simplifyUnits . foldUnits . catMaybes) <$> mapM eachName unitGroup
        return (lhsU, solvedUnit)

  mapM eachGroup unitGroups


inferVariablesSBV :: Constraints -> [(VV, UnitInfo)]
inferVariablesSBV cons = solvedVars
  where
    -- We are only interested in reporting the solutions to variables.
    solvedVars = [ (vv, unit) | v@(UnitVar vv, unit) <- unitAssignments ] ++
                 [ (vv, unit) | v@(UnitParamVarAbs (_, vv), unit) <- unitAssignments ]
    unitAssignments = genUnitAssignmentsSBV cons


----

-- Unification: we solve under the assumption of more concrete RHS
--   units but if we find in the results that certain ParamPosAbs are
--   still left 'free' in some sense then they are potentially
--   parametric?

--   indeed, such ParamPosAbs variables are valid with any value it seems. underdetermined.
--   how many values is enough ? 2?
--   can I subset the underdetermined equations?

-- yes, there are multiple values for both parametric & critical-vars
-- the multiple values are equationally linked to each other if the variables are
-- could potentially identify all parametric variables in parallel by constraining to prime numbers
