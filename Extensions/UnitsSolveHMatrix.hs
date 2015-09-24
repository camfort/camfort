module Extensions.UnitsSolveHMatrix
  ( rref, rrefMatrices, convertToHMatrix, convertFromHMatrix, dispf, Units, lu, rank, takeRows )
where
import Numeric.LinearAlgebra
import Data.Packed.Matrix (fromBlocks)
import qualified Data.Matrix as Old (nrows, ncols, toList, Matrix, fromList)
import Foreign.Storable (Storable)
import Data.List (findIndex, nub)
import Data.Maybe (fromMaybe)
import Extensions.UnitsEnvironment (LinearSystem, UnitConstant(..))
import Language.Fortran (MeasureUnit)

rref :: (Eq a, Fractional a, Product a) => Matrix a -> Matrix a
rref a = snd $ rrefMatrices' a 0 0 []

rrefMatrices :: (Eq a, Fractional a, Product a) => Matrix a -> [Matrix a]
rrefMatrices a = fst $ rrefMatrices' a 0 0 []

rrefMatrix :: (Eq t, Fractional t, Product t) => Matrix t -> Matrix t
rrefMatrix a = foldr (<>) (ident (rows a)) . fst $ rrefMatrices' a 0 0 []

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


convertMatrixToHMatrix :: (Enum a) => Old.Matrix a -> Matrix Double
convertMatrixToHMatrix a = (Old.nrows a >< Old.ncols a) . map (toEnum . fromEnum) $ Old.toList a

convertHMatrixToMatrix :: (Enum a) => Matrix Double -> Old.Matrix a
convertHMatrixToMatrix a = Old.fromList (rows a) (cols a) . map (toEnum . fromEnum) . toList $ flatten a

type Units = [MeasureUnit]
convertToHMatrix :: LinearSystem -> (Matrix Double, Units)
convertToHMatrix (a, ucs) = (fromBlocks [[a', unitA]], units)
  where
    a'       = convertMatrixToHMatrix a
    m        = cols a'
    units    :: [String]
    units    = nub . (ucs >>=) $ \ uc -> case uc of
                   Unitful us -> map fst us
                   _          -> []
    toDouble :: Enum a => a -> Double
    toDouble = toEnum . fromEnum
    unitA    :: Matrix Double
    unitA    = fromLists . flip map ucs $ \ uc -> case uc of
                   Unitful us -> flip map units (toDouble . fromMaybe 0 . flip lookup us)
                   _          -> map (const 0) units

convertFromHMatrix :: Enum a => (Matrix Double, [MeasureUnit]) -> (Old.Matrix a, [UnitConstant])
convertFromHMatrix (a, units) = (a', ucs)
  where
    ulen  = length units
    a'    = convertHMatrixToMatrix $ takeColumns (cols a - ulen) a
    unitA = dropColumns (cols a - ulen) a
    ucs   :: [UnitConstant]
    ucs   = flip map (toLists unitA) (Unitful . filter ((/= 0) . snd) . zip units . map (toEnum . fromEnum))
