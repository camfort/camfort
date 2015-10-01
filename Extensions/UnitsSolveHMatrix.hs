module Extensions.UnitsSolveHMatrix
  ( rref, rrefMatrices, convertToHMatrix, convertFromHMatrix, dispf, Units, lu, rank, takeRows )
where

import Data.Ratio
import Debug.Trace (trace)
import Numeric.LinearAlgebra
import Data.Packed.Matrix (fromBlocks)
import qualified Data.Matrix as Old (nrows, ncols, toList, Matrix, fromList)
import Foreign.Storable (Storable)
import Data.List (findIndex, nub, sort)
import Data.Maybe (fromMaybe)
import Extensions.UnitsEnvironment (LinearSystem, UnitConstant(..))
import Language.Fortran (MeasureUnit)

-- | Reduced Row Echelon Form
rref :: (Eq a, Fractional a, Product a) => Matrix a -> Matrix a
rref a = snd $ rrefMatrices' a 0 0 []

-- | List of matrices that when multiplied transform input into
-- Reduced Row Echelon Form
rrefMatrices :: (Eq a, Fractional a, Product a) => Matrix a -> [Matrix a]
rrefMatrices a = fst $ rrefMatrices' a 0 0 []

-- | Single matrix that transforms input into Reduced Row Echelon form
-- when multiplied to the original.
rrefMatrix :: (Eq t, Fractional t, Product t) => Matrix t -> Matrix t
rrefMatrix a = foldr (<>) (ident (rows a)) . fst $ rrefMatrices' a 0 0 []

-- worker function
-- invariant: the matrix a is in rref except within the submatrix (j-k,j) to (n,n)
rrefMatrices' a j k mats
  | j - k == n            = (mats, a)
  | j     == m            = (mats, a)
  | a @@> (j - k, j) == 0 = case findIndex (/= 0) below of
    Nothing -> rrefMatrices' a (j + 1) (k + 1) mats  -- column is all 0s below current row
    Just i' -> rrefMatrices' (swapMat <> a) j k (swapMat:mats) -- swap and try again
      where i       = j - k + i'
            swapMat = elemRowSwap n i (j - k)
  | otherwise             = rrefMatrices' a' (j + 1) k (ms ++ mats)
  where
    n     = rows a
    m     = cols a
    below = concat . toLists $ subMatrix (j - k, j) (n - (j - k), 1) a
    a'    = foldr (<>) a ms
    ms    = adds ++ (if a @@> (j - k, j) /= 1 then [elemRowMult n (j - k) (recip (a @@> (j - k, j)))] else [])
    adds  = [0..(n - 1)] >>= f
    f i | i == j - k        = []
        | a @@> (i, j) == 0 = []
        | otherwise         = [elemRowAdd n i (j - k) (- (a @@> (i, j)))]

-- 'Elementary row operation' matrices
elemRowMult n i k
  | 0 <= i && i < n = diag (fromList (replicate i 1.0 ++ [k] ++ replicate (n - i - 1) 1.0))
  | otherwise       = undefined

elemRowAdd n i j k
  | i < 0 || i >= n = undefined
  | j < 0 || j >= n = undefined
  | otherwise       = flip mapMatrixWithIndex (ident n) $ \ p x -> if (i, j) == p then k else x

elemRowSwap n i j
  | i == j          = ident n
  | i < 0 || i >= n = undefined
  | j < 0 || j >= n = undefined
  | i > j           = elemRowSwap n j i
  | otherwise       = extractRows ([0..i-1] ++ [j] ++ [i+1..j-1] ++ [i] ++ [j+1..n-1]) $ ident n


--------------------------------------------------

type Units = [MeasureUnit]

-- | Convert a LinearSystem into an hmatrix and a list of units that are used
convertToHMatrix :: LinearSystem -> (Matrix Double, Units)
convertToHMatrix (a, ucs) = (fromBlocks [[a', unitA]], units)
  where
    s = show ucs
    a'       = convertMatrixToHMatrix a
    m        = cols a'
    units    = ucsToUnits ucs
    unitA    = unitsToUnitA ucs units

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
