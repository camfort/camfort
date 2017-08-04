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
import Data.List ((\\), isPrefixOf, findIndex, partition, sortBy, group, groupBy, tails, transpose, nub)
import Data.Generics.Uniplate.Operations (rewrite)
import Debug.Trace (trace, traceShowM, traceM)
import Control.Monad
import Control.Monad.ST
import Control.Arrow (first, second)
import qualified Data.Map.Strict as M
import qualified Data.Array as A
import System.IO.Unsafe (unsafePerformIO)
import Data.SBV ( transcript, SatResult(..), SMTResult(Unknown), Symbolic, SBool, SInteger, SBV
                , satWith, z3, getModelDictionary, fromCW, true, namedConstraint, (.==)
                , sInteger, literal, bAnd, Predicate, getModelValue )
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

-- | Returns list of formerly-undetermined variables and their units.
inferVariablesSBV' :: Constraints -> [(VV, UnitInfo)]
inferVariablesSBV' cons = unsafePerformIO $ do
  unitAssignments <- genUnitAssignmentsSBV cons
  -- Find the rows corresponding to the distilled "unit :: var"
  -- information for ordinary (non-polymorphic) variables.
  return [ (var, units) | (UnitVar var, units) <- unitAssignments ]

-- Convert a set of constraints into a matrix of co-efficients, and a
-- reverse mapping of column numbers to units.
constraintsToLHSRHSMatrix :: Constraints -> (H.Matrix Double, H.Matrix Double, A.Array Int UnitInfo, A.Array Int UnitInfo)
constraintsToLHSRHSMatrix cons = (lhsM, rhsM, lhsCols, rhsCols)
  where
    -- convert each constraint into the form (lhs, rhs)
    consPairs       = flattenConstraints cons
    -- ensure terms are on the correct side of the equal sign
    shiftedCons     = map shiftTerms consPairs
    lhs             = map fst shiftedCons
    rhs             = map snd shiftedCons
    (lhsM, lhsCols) = flattenedToMatrix lhs
    (rhsM, rhsCols) = flattenedToMatrix rhs

-- | Raw units-assignment pairs.
genUnitAssignmentsSBV :: [Constraint] -> IO [(UnitInfo, UnitInfo)]
genUnitAssignmentsSBV cons = do
    let (lhsM, rhsM, lhsCols, rhsCols) = constraintsToLHSRHSMatrix cons
    units <- solveSMT (lhsM, rhsM, lhsCols, rhsCols)

    let unitPows = map (second (concatMap flattenUnits)) units

    -- Variables to the left, unit names to the right side of the equation.
    let unitAssignments = map (second (foldUnits . map negatePosAbs)) unitPows
    return unitAssignments
  where
    foldUnits units
      | null units = UnitlessVar
      | otherwise  = foldl1 UnitMul units


solveSMT :: (H.Matrix Double, H.Matrix Double, A.Array Int UnitInfo, A.Array Int UnitInfo)
         -> IO [(UnitInfo, [UnitInfo])]
solveSMT (lhsM, rhsM, lhsCols, rhsCols) = do

    satResult <- satWith
                   z3{transcript=Just "model.smt2"} -- SMT-LIB dump
                   predicate

    case satResult of
      SatResult (Unknown _ model) -> putStrLn $ "Unknown (SAT): " ++ show model
      _ -> return ()

    let dict = getModelDictionary satResult

    let vars = filter (\x -> "#<Var" `isPrefixOf` show x) (A.elems lhsCols)

    let get k = case M.lookup k dict of Just val -> fromCW val :: Integer


    let x = map (\v -> filter (isPrefixOf (show v)) (M.keys dict)) vars
    -- print x
    -- x = [["#<Var trivial_dist1>-0","#<Var trivial_dist1>-1"],["#<Var trivial_sum2>-0","#<Var trivial_sum2>-1"],["#<Var trivial_time3>-0","#<Var trivial_time3>-1"],["#<Var trivial_y4>-0","#<Var trivial_y4>-1"]]

    let y = map (map get) x
    -- print y
    -- [("#<Var trivial_dist1>",[1,0]),("#<Var trivial_sum2>",[1,-1]),("#<Var trivial_time3>",[0,1]),("#<Var trivial_y4>",[0,0])]

   -- z
    let z :: [(UnitInfo, [(UnitInfo, Integer)])] =
         zip vars $ map (zip (A.elems rhsCols)) y

    -- print satResult
    -- print dict
    return $ map (second (map (\(u, c) -> UnitPow u $ fromInteger c))) z
  where
    predicate :: Symbolic SBool
    predicate = do
      -- Generate names (constrained to their assigned values) for RHS
      rhsVars <- zipWithM generateRHSRows [1..] (H.toRows rhsM)
      -- Generate LHS variables
      lhsVars <- mkExistVarsNamed lhsColNamesNumbered
      -- Make constraints
      genEqConstraints lhsVars (H.toRows lhsM) rhsVars
      return true

    genEqConstraints :: [[(String, SInteger)]] -- lhs variables (len lc * len rc)
                     -> [H.Vector Double]       -- matrix lhs coeffecients (len r)
                     -> [[(String, SInteger)]]  -- matrix rhs variables    (len r)
                     -> Symbolic ()
    genEqConstraints lhsVars lhsCoeffss rhsVarss = do
      zipWithM' (genEqConstraints' lhsVars) lhsCoeffss rhsVarss
      return ()

    genEqConstraints' :: [[(String, SInteger)]] -- lhs variables (len lc * len rc)
                      -> H.Vector Double        -- row lhs coeffections (len lc)
                      -> [(String, SInteger)]   -- row rhs variables (len rc)
                      -> Symbolic ()
    genEqConstraints' lhsVars lhsCoeffs rhsVars = do
      zipWithM' (genUnitConstraint lhsCoeffs) (transpose lhsVars) rhsVars
      return ()

    genUnitConstraint :: H.Vector Double      -- row lhs coeffects (len lc)
                       -> [(String, SInteger)] -- lhs variables (len lc)
                       -> (String, SInteger)   -- rhs variable
                       -> Symbolic ()
    genUnitConstraint lhsCoeffs lhsVars (vRname, vR) = do
      let lhs = sum $ zipWith (\c (_, vL) -> cast c * vL) (H.toList lhsCoeffs) lhsVars
      let lhsName = foldr1 (++) $
                      zipWith (\c (vLname, _) -> show c ++ " * " ++ vLname)
                        (H.toList lhsCoeffs) lhsVars
      namedConstraint (lhsName ++ " == " ++ vRname) (lhs .== vR)
      return ()

    generateRHSRows :: Int -> H.Vector Double -> Symbolic [(String, SBV Integer)]
    generateRHSRows r v = do
      let cs = H.toList v
      vars <- mapM (\n -> do let name = "rhs-" ++ show r ++ "-" ++ show n
                             var <- sInteger name
                             return (name, var)) [1..length cs]
      zipWithM' (\(vname, v) c -> namedConstraint (vname ++ " == " ++ show c) (v .== cast c)) vars cs
      return vars

    cast :: Double -> SInteger
    cast = literal . round

    mkExistVarsNamed :: [[String]] -> Symbolic [[(String, SInteger)]]
    mkExistVarsNamed = mapM (mapM (\x -> do { v <- sInteger x; return (x, v) }))

    lhsColNamesNumbered :: [[String]]
    lhsColNamesNumbered = transpose $ map (\n -> map (++ show n) lhsColNames) [0..cols rhsM-1]

    lhsColNames :: [String]
    lhsColNames = [ identif ++ "-" -- ++ show row ++ "-"
                  | (identif, row) <- zip (map show $ A.elems lhsCols) [0..]
                  ]


    zipWithM' f xs ys
      | length xs /= length ys =
          error $ "not of same length " ++ show xs ++ " and " ++ show ys
      | otherwise = zipWithM f xs ys
          ----------------------------------------------

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

-- Convert a set of constraints into a matrix of co-efficients, and a
-- reverse mapping of column numbers to units.
constraintsToMatrix :: Constraints -> (H.Matrix Double, A.Array Int UnitInfo)
constraintsToMatrix cons = (augM, A.listArray (0, length colElems - 1) colElems)
  where
    -- convert each constraint into the form (lhs, rhs)
    consPairs       = flattenConstraints cons
    -- ensure terms are on the correct side of the equal sign
    shiftedCons     = map shiftTerms consPairs
    lhs             = map fst shiftedCons
    rhs             = map snd shiftedCons
    (lhsM, lhsCols) = flattenedToMatrix lhs
    (rhsM, rhsCols) = flattenedToMatrix rhs
    colElems        = A.elems lhsCols ++ A.elems rhsCols
    augM            = if rows rhsM == 0 || cols rhsM == 0 then lhsM else fromBlocks [[lhsM, rhsM]]

constraintsToMatrices :: Constraints -> (H.Matrix Double, H.Matrix Double, A.Array Int UnitInfo, A.Array Int UnitInfo)
constraintsToMatrices cons = (lhsM, rhsM, lhsCols, rhsCols)
  where
    -- convert each constraint into the form (lhs, rhs)
    consPairs       = filter (uncurry (/=)) $ flattenConstraints cons
    -- ensure terms are on the correct side of the equal sign
    shiftedCons     = map shiftTerms consPairs
    lhs             = map fst shiftedCons
    rhs             = map snd shiftedCons
    (lhsM, lhsCols) = flattenedToMatrix lhs
    (rhsM, rhsCols) = flattenedToMatrix rhs
    augM            = if rows rhsM == 0 || cols rhsM == 0 then lhsM else fromBlocks [[lhsM, rhsM]]

-- [[UnitInfo]] is a list of flattened constraints
flattenedToMatrix :: [[UnitInfo]] -> (H.Matrix Double, A.Array Int UnitInfo)
flattenedToMatrix cons = (m, A.array (0, numCols - 1) (map swap uniqUnits))
  where
    m = runSTMatrix $ do
          m <- newMatrix 0 numRows numCols
          -- loop through all constraints
          forM_ (zip cons [0..]) $ \ (unitPows, row) -> do
            -- write co-efficients for the lhs of the constraint
            forM_ unitPows $ \ (UnitPow u k) -> do
              case M.lookup u colMap of
                Just col -> readMatrix m row col >>= (writeMatrix m row col . (+k))
                _        -> return ()
          return m
    -- identify and enumerate every unit uniquely
    uniqUnits = flip zip [0..] . map head . group . sortBy colSort $ [ u | UnitPow u _ <- concat cons ]
    -- map units to their unique column number
    colMap    = M.fromList uniqUnits
    numRows   = length cons
    numCols   = M.size colMap

negateCons = map (\ (UnitPow u k) -> UnitPow u (-k))

negatePosAbs (UnitPow (UnitParamPosAbs x) k) = UnitPow (UnitParamPosAbs x) (-k)
negatePosAbs u                               = u

colSort (UnitLiteral i) (UnitLiteral j)         = compare i j
colSort (UnitLiteral _) _                       = LT
colSort _ (UnitLiteral _)                       = GT
colSort (UnitParamPosAbs x) (UnitParamPosAbs y) = compare x y
colSort (UnitParamPosAbs _) _                   = GT
colSort _ (UnitParamPosAbs _)                   = LT
colSort x y                                     = compare x y

--------------------------------------------------

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

foldUnits units
  | null units = UnitlessVar
  | otherwise  = foldl1 UnitMul units

--------------------------------------------------

type Z3 a = Symbolic a
type Symbol = SInteger

type UnitZ3Map = M.Map (UnitInfo, UnitInfo) Symbol

type LhsUnit = UnitInfo
type RhsUnit = UnitInfo
type UnitInfoNameMap = M.Map String (LhsUnit, RhsUnit)

gatherRhsUnitInfoNames :: [[UnitInfo]] -> [(String, RhsUnit)]
gatherRhsUnitInfoNames = concatMap eachRow
  where
    eachRow               = map eachCol

    eachCol (UnitPow u _) = (show u, u)
    eachCol u             = (show u, u)

gatherLhsUnitInfoNames :: (String, RhsUnit) -> [[UnitInfo]] -> [(String, (LhsUnit, RhsUnit))]
gatherLhsUnitInfoNames (rhsName, rhsUnit) = concatMap eachRow
  where
    eachRow               = map eachCol

    eachCol (UnitPow u _) = (show u ++ "_" ++ rhsName, (u, rhsUnit))
    eachCol u             = (show u ++ "_" ++ rhsName, (u, rhsUnit))

gatherUnitInfoNameMap :: [([UnitInfo], [UnitInfo])] -> UnitInfoNameMap
gatherUnitInfoNameMap shiftedCons = M.fromListWith (curry fst) lhsNames
  where
    lhsNames = concatMap (flip gatherLhsUnitInfoNames lhsRows) rhsNames
    lhsRows  = map fst shiftedCons

    rhsNames = gatherRhsUnitInfoNames rhsRows
    rhsRows  = map snd shiftedCons

inferVariablesSBV :: Constraints -> [(VV, UnitInfo)]
inferVariablesSBV cons = unsafePerformIO $ do
  let shiftedCons :: [([UnitInfo], [UnitInfo])]
      shiftedCons = map shiftTerms $ flattenConstraints cons

  let uiNameMap = gatherUnitInfoNameMap shiftedCons

  let genVar :: String -> Symbolic (String, SInteger)
      genVar name = (name,) <$> sInteger name

  let rhsNames :: [(String, UnitInfo)]
      rhsNames = gatherRhsUnitInfoNames (map snd shiftedCons)

  -- start off with every RHS mapped to a power of zero.
  let baseRhsMap = M.fromList [ (name, 0) | (name, _) <- rhsNames ]

  let pred :: Predicate
      pred = do
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
        let eachRhs :: [UnitInfo] -> (String, Integer) -> Maybe SBool
            eachRhs lhs (rhsName, rhsPow)
              | null lhsTerms = Nothing
              | otherwise     = Just (sum lhsTerms .== literal rhsPow)
              where
                -- lhsTerms = [lhs1_RHS * pow1, lhs2_RHS * pow2, ..., lhsN_RHS powN]
                lhsTerms :: [SInteger]
                lhsTerms = [ lhsSym * literal lhsPow | lhs_i <- lhs
                                                     , let (lhsSym, lhsPow) = getLhsSymbol rhsName lhs_i ]

        -- for each constraint having a set of LHS terms and a set of RHS terms:
        let eachConstraint :: ([UnitInfo], [UnitInfo]) -> [SBool]
            eachConstraint (lhs, rhs) = res
              where
                msg       = "eachConstraint " ++ show (lhs, rhs) ++ " = " ++ show res
                res       = catMaybes . map (eachRhs lhs) $ rhsPowers
                -- map every RHS to its corresponding power (including 0 for those not mentioned)
                rhsPowers = M.toList . M.unionWith (+) baseRhsMap . M.fromList . map getRhsDetails $ rhs

        -- conjunction of all constraints
        return . bAnd $ concatMap eachConstraint shiftedCons

  satResult <- satWith z3 { transcript = Just "mrd.smt2" } -- SMT-LIB dump
               pred

  -- Interpret results.

  -- The uiNameMap stores the mapping between each SInteger name and
  -- its corresponding (lhsU, rhsU). Therefore we sort and group each
  -- entry by its lhsU, and then check the solved integer value of the
  -- SInteger name. That solved integer value corresponds to rhsU raised
  -- to that power. Take all of the rhsUs, raised to their respective
  -- powers, and combine them into a single UnitMul for each lhsU.

  let lhsU = fst . snd
  let unitGroups = groupBy ((==) `on` lhsU) . sortBy (comparing lhsU) $ M.toList uiNameMap

  -- unitGroups =
  --   [ [(name1_1, (lhs1, rhs1)), (name1_2, (lhs1, rhs2)), ...]
  --   , [(name2_1, (lhs2, rhs1)), (name2_2, (lhs2, rhs2)), ...]
  --   , ...]

  let eachName (lhsName, (lhsU, rhsU))
        | x == 0    = Nothing
        | otherwise = Just $ UnitPow rhsU (fromInteger x)
        where x = error "inferVariables missing piece of model" `fromMaybe` getModelValue lhsName satResult

  let eachGroup unitGroup = (lhsU, solvedUnit)
        where
          (_, (lhsU, _)):_ = unitGroup -- grouped by lhsU, so pick out one of them
          solvedUnit = simplifyUnits . foldUnits . catMaybes $ map eachName unitGroup

  let solvedUnits = map eachGroup unitGroups

  -- We are only interested in reporting the solutions to variables.
  let solvedVars = [ (vv, unit) | v@(UnitVar vv, unit) <- solvedUnits ]
  return solvedVars
