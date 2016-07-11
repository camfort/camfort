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
{-# LANGUAGE FlexibleContexts #-}

module Camfort.Specification.Units.SolveHMatrix
  ( rref, rrefMatrices, convertToHMatrix, convertFromHMatrix, isInconsistentRREF
  , dispf, Units, lu, rank, takeRows )
where

import Data.Ratio
import Debug.Trace (trace)
import Numeric.LinearAlgebra (
    atIndex, (<>), (><), rank, (?), toLists, toList, fromLists, fromList, rows, cols,
    Matrix, takeRows, takeColumns, dropRows, dropColumns, subMatrix, diag, build, fromBlocks,
    ident, flatten, lu, dispf
  )
import Numeric.LinearAlgebra.Devel (
    newMatrix, writeMatrix, runSTMatrix
  )
import Control.Monad (filterM)
import Control.Monad.ST
import qualified Data.Matrix as Old (nrows, ncols, toList, Matrix, fromList)
import Foreign.Storable (Storable)
import Data.List (findIndex, nub, sort, (\\))
import Data.Maybe (fromMaybe)
import Camfort.Specification.Units.Environment (LinearSystem, UnitConstant(..))
import Language.Fortran (MeasureUnit)

-- | Returns True iff the given matrix in reduced row echelon form
-- represents an inconsistent system of linear equations
isInconsistentRREF a = a @@> (rows a - 1, cols a - 1) == 1 && rank (takeColumns (cols a - 1) (dropRows (rows a - 1) a))== 0

-- | Returns given matrix transformed into Reduced Row Echelon Form
rref :: Matrix Double -> Matrix Double
rref a = snd $ rrefMatrices' a 0 0 []

-- | List of matrices that when multiplied transform input into
-- Reduced Row Echelon Form
rrefMatrices :: Matrix Double -> [Matrix Double]
rrefMatrices a = fst $ rrefMatrices' a 0 0 []

-- | Single matrix that transforms input into Reduced Row Echelon form
-- when multiplied to the original.
rrefMatrix :: Matrix Double -> Matrix Double
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
getColumnBelow a (i, j) = concat . toLists $ subMatrix (i, j) (n - i, 1) a
  where n = rows a

-- 'Elementary row operation' matrices
elemRowMult n i k = diag (fromList (replicate i 1.0 ++ [k] ++ replicate (n - i - 1) 1.0))


elemRowAdd :: Int -> Int -> Int -> Double -> Matrix Double
elemRowAdd n i j k = runSTMatrix $ do
      m <- newMatrix 0 n n
      sequence [ writeMatrix m i' i' 1 | i' <- [0 .. (n - 1)] ]
      writeMatrix m i j k
      return m

elemRowAdd_spec n i j k
  | i < 0 || i >= n = undefined
  | j < 0 || j >= n = undefined
  | otherwise       = build n n f
  where
    f (i', j') | i == i' && j == j' = k
               | i' == j'           = 1
               | otherwise          = 0

elemRowSwap n i j
  | i == j          = ident n
  | i > j           = elemRowSwap n j i
  | otherwise       = extractRows ([0..i-1] ++ [j] ++ [i+1..j-1] ++ [i] ++ [j+1..n-1]) $ ident n


--------------------------------------------------

type Units = [MeasureUnit]

-- | Convert a LinearSystem into an hmatrix and a list of units that are used
convertToHMatrix :: LinearSystem -> Either [Int] (Matrix Double, Units)
convertToHMatrix (a, ucs) = case findInconsistentRows a' augA of
                              [] -> Right (augA, units)
                              ns -> Left ns
  where
    a'       = convertMatrixToHMatrix a
    m        = cols a'
    units    = ucsToUnits ucs
    unitA    = unitsToUnitA ucs units
    augA     = fromBlocks [[a', unitA]]

-- | Convert an hmatrix and the list of units used back into a LinearSystem
convertFromHMatrix :: (Matrix Double, [MeasureUnit]) -> LinearSystem
convertFromHMatrix (a, units) = (a', ucs')
  where
    ulen  = length units
    a'    = convertHMatrixToMatrix (takeColumns (cols a - ulen) a)
    unitA = dropColumns (cols a - ulen) a
    ucs   = unitAToUcs unitA units
    -- special case: when there are no units, ensure the empty list is replaced with [Unitful [] ...]
    ucs'  = if null ucs then replicate (rows a) (Unitful []) else ucs


-- Worker functions:

convertMatrixToHMatrix :: Old.Matrix Rational -> Matrix Double
convertMatrixToHMatrix a = (Old.nrows a >< Old.ncols a) . map toDouble $ Old.toList a

convertHMatrixToMatrix :: Matrix Double -> Old.Matrix Rational
convertHMatrixToMatrix a = Old.fromList (rows a) (cols a) . map fromDouble . toList $ flatten a

toDouble :: Rational -> Double
toDouble = fromRational

fromDouble :: Double -> Rational
fromDouble = toRational

unitsToUnitA :: [UnitConstant] -> Units -> Matrix Double
unitsToUnitA ucs units = unitA
  where
    unitA = fromLists . flip map ucs $ \ uc -> case uc of
              Unitful us -> flip map units (toDouble . fromMaybe 0 . flip lookup us)
              _          -> map (const 0) units

ucsToUnits :: [UnitConstant] -> Units
ucsToUnits ucs = sort . nub . (ucs >>=) $ \ uc -> case uc of
                   Unitful us -> map fst us
                   _          -> []

unitAToUcs :: Matrix Double -> Units -> [UnitConstant]
unitAToUcs unitA units =
  flip map (toLists unitA) (Unitful . filter ((/= 0) . snd) . zip units . map fromDouble)

findInconsistentRows :: Matrix Double -> Matrix Double -> [Int]
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
