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
-}

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camfort.Specification.Units.InferenceBackend
  ( inconsistentConstraints, criticalVariables, inferVariables
  -- mainly for debugging:
  , flattenUnits, constraintsToMatrix, rref, isInconsistentRREF )
where

import Data.Tuple (swap)
import Data.Maybe (maybeToList)
import Data.List ((\\), findIndex, partition, sortBy, group)
import Data.Generics.Uniplate.Operations
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.ST
import Control.Arrow (first, second)
import qualified Data.Map as M
import qualified Data.Array as A

import Camfort.Analysis.Annotations
import Camfort.Specification.Units.Environment

import Numeric.LinearAlgebra (
    atIndex, (<>), (><), rank, (?), toLists, toList, fromLists, fromList, rows, cols,
    takeRows, takeColumns, dropRows, dropColumns, subMatrix, diag, build, fromBlocks,
    ident, flatten, lu, dispf
  )
import qualified Numeric.LinearAlgebra as H
import Numeric.LinearAlgebra.Devel (
    newMatrix, readMatrix, writeMatrix, runSTMatrix
  )

import qualified Debug.Trace as D

--------------------------------------------------

-- | Returns just the list of constraints that were identified as
-- being possible candidates for inconsistency, if there is a problem.
inconsistentConstraints :: Constraints -> Maybe Constraints
inconsistentConstraints cons
  | null inconsists = Nothing
  | otherwise       = Just [ con | (con, i) <- zip cons [0..], i `elem` inconsists ]
  where
    (unsolvedM, inconsists, colA) = constraintsToMatrix cons

--------------------------------------------------

-- | Identifies the variables that need to be annotated in order for
-- inference or checking to work.
criticalVariables :: Constraints -> [UnitInfo]
criticalVariables cons = filter (not . isUnitName) $ map (colA A.!) criticalIndices
  where
    (unsolvedM, inconsists, colA) = constraintsToMatrix cons
    solvedM                       = rref unsolvedM
    uncriticalIndices             = concatMap (maybeToList . findIndex (/= 0)) $ H.toLists solvedM
    criticalIndices               = A.indices colA \\ uncriticalIndices
    isUnitName (UnitName _)       = True; isUnitName _ = False

--------------------------------------------------

-- | Returns list of formerly-undetermined variables and their units.
inferVariables :: Constraints -> [(String, UnitInfo)]
inferVariables cons = [ (var, if null units then UnitlessVar else foldl1 UnitMul units)
                      | ([UnitPow (UnitVar var) k], units) <- map (partition (not . isUnitName)) unitPows
                      , k `approxEq` 1 ]
  where
    (unsolvedM, inconsists, colA)       = constraintsToMatrix cons
    solvedM                             = rref unsolvedM
    cols                                = A.elems colA
    unitPows                            = map (concatMap flattenUnits . zipWith UnitPow cols) (H.toLists solvedM)
    isUnitName (UnitPow (UnitName _) _) = True; isUnitName _ = False

--------------------------------------------------

simplifyConstraints = map (\ (ConEq u1 u2) -> (flattenUnits u1, flattenUnits u2))

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

--------------------------------------------------

-- Convert a set of constraints into a matrix of co-efficients, and a
-- reverse mapping of column numbers to units.
constraintsToMatrix :: Constraints -> (H.Matrix Double, [Int], A.Array Int UnitInfo)
constraintsToMatrix cons = (augM, inconsists, A.listArray (0, length colElems - 1) colElems)
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
    inconsists      = findInconsistentRows lhsM augM

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

colSort (UnitLiteral i) (UnitLiteral j) = compare i j
colSort (UnitLiteral _) _               = LT
colSort _ (UnitLiteral _)               = GT
colSort x y                             = compare x y

--------------------------------------------------

-- | Translate all constraints into a LHS, RHS side of units.
flattenConstraints :: Constraints -> [([UnitInfo], [UnitInfo])]
flattenConstraints = map (\ (ConEq u1 u2) -> (flattenUnits u1, flattenUnits u2))

-- | Shift UnitNames to the RHS, and all else to the LHS.
shiftTerms :: ([UnitInfo], [UnitInfo]) -> ([UnitInfo], [UnitInfo])
shiftTerms (lhs, rhs) = (lhsOk ++ negateCons rhsShift, rhsOk ++ negateCons lhsShift)
  where
    (lhsOk, lhsShift) = partition (not . isUnitName) lhs
    (rhsOk, rhsShift) = partition isUnitName rhs
    isUnitName (UnitPow (UnitName _) _) = True; isUnitName _ = False

--------------------------------------------------
-- Matrix solving functions based on HMatrix

-- | Returns True iff the given matrix in reduced row echelon form
-- represents an inconsistent system of linear equations
isInconsistentRREF a = a @@> (rows a - 1, cols a - 1) == 1 && rank (takeColumns (cols a - 1) (dropRows (rows a - 1) a))== 0

-- | Returns given matrix transformed into Reduced Row Echelon Form
rref :: H.Matrix Double -> H.Matrix Double
rref a = snd $ rrefMatrices' a 0 0 []

-- | List of matrices that when multiplied transform input into
-- Reduced Row Echelon Form
rrefMatrices :: H.Matrix Double -> [H.Matrix Double]
rrefMatrices a = fst $ rrefMatrices' a 0 0 []

-- | Single matrix that transforms input into Reduced Row Echelon form
-- when multiplied to the original.
rrefMatrix :: H.Matrix Double -> H.Matrix Double
rrefMatrix a = foldr (<>) (ident (rows a)) . fst $ rrefMatrices' a 0 0 []

-- worker function
-- invariant: the matrix a is in rref except within the submatrix (j-k,j) to (n,n)
rrefMatrices' a j k mats
  -- Base cases:
  | j - k == n            = (mats, a)
  | j     == m            = (mats, a)

  -- When we haven't yet found the first non-zero number in the row, but we really need one:
  | a @@> (j - k, j) == 0 = case findIndex (/= 0) below of
    -- this column is all 0s below current row, must move onto the next column
    Nothing -> rrefMatrices' a (j + 1) (k + 1) mats
    -- we've found a row that has a non-zero element that can be swapped into this row
    Just i' -> rrefMatrices' (swapMat <> a) j k (swapMat:mats)
      where i       = j - k + i'
            swapMat = elemRowSwap n i (j - k)

  -- We have found a non-zero cell at (j - k, j), so transform it into
  -- a 1 if needed using elemRowMult, and then clear out any lingering
  -- non-zero values that might appear in the same column, using
  -- elemRowAdd:
  | otherwise             = rrefMatrices' a2 (j + 1) k mats2
  where
    n     = rows a
    m     = cols a
    below = getColumnBelow a (j - k, j)

    -- scale the row if the cell is not already equal to 1
    erm    = elemRowMult n (j - k) (recip (a @@> (j - k, j)))
    (a1, mats1) = if a @@> (j - k, j) /= 1 then
                    (erm <> a, erm:mats)
                  else (a, mats)

    -- Locate any non-zero values in the same column as (j - k, j) and
    -- cancel them out. Optimisation: instead of constructing a
    -- separate elemRowAdd matrix for each cancellation that are then
    -- multiplied together, simply build a single matrix that cancels
    -- all of them out at the same time, using the ST Monad.
    findAdds i m ms = (new <> m, new:ms)
      where
        new = runSTMatrix $ do
          new <- newMatrix 0 n n
          sequence [ writeMatrix new i' i' 1 | i' <- [0 .. (n - 1)] ]
          let f i | i >= n            = return ()
                  | i == j - k        = f (i + 1)
                  | a @@> (i, j) == 0 = f (i + 1)
                  | otherwise         = writeMatrix new i (j - k) (- (a @@> (i, j)))
                                        >> f (i + 1)
          f 0
          return new
    (a2, mats2) = findAdds 0 a1 mats1

-- Get a list of values that occur below (i, j) in the matrix a.
getColumnBelow a (i, j) = concat . H.toLists $ subMatrix (i, j) (n - i, 1) a
  where n = rows a

-- 'Elementary row operation' matrices
elemRowMult :: Int -> Int -> Double -> H.Matrix Double
elemRowMult n i k = diag (H.fromList (replicate i 1.0 ++ [k] ++ replicate (n - i - 1) 1.0))

elemRowAdd :: Int -> Int -> Int -> Double -> H.Matrix Double
elemRowAdd n i j k = runSTMatrix $ do
      m <- newMatrix 0 n n
      sequence [ writeMatrix m i' i' 1 | i' <- [0 .. (n - 1)] ]
      writeMatrix m i j k
      return m

elemRowSwap :: Int -> Int -> Int -> H.Matrix Double
elemRowSwap n i j
  | i == j          = ident n
  | i > j           = elemRowSwap n j i
  | otherwise       = extractRows ([0..i-1] ++ [j] ++ [i+1..j-1] ++ [i] ++ [j+1..n-1]) $ ident n


--------------------------------------------------

-- Worker functions:

toDouble :: Rational -> Double
toDouble = fromRational

fromDouble :: Double -> Rational
fromDouble = toRational

findInconsistentRows :: H.Matrix Double -> H.Matrix Double -> [Int]
findInconsistentRows coA augA = [0..(rows augA - 1)] \\ consistent
  where
    consistent = head (filter (tryRows coA augA) (pset ( [0..(rows augA - 1)])) ++ [[]])

    -- Rouché–Capelli theorem is that if the rank of the coefficient
    -- matrix is not equal to the rank of the augmented matrix then
    -- the system of linear equations is inconsistent.
    tryRows coA augA ns = (rank coA' == rank augA')
      where
        coA'  = extractRows ns coA
        augA' = extractRows ns augA

    pset = filterM (const [True, False])

extractRows = flip (?) -- hmatrix 0.17 changed interface
m @@> i = m `atIndex` i
