{-# LANGUAGE ImplicitParams #-}

module Extensions.UnitsSolve where

import Data.List
import Data.Matrix
import qualified Data.Vector as V
import Numeric.LinearAlgebra.LAPACK
import qualified Data.Packed.Matrix as LM
import Control.Exception
import System.IO.Unsafe


import Language.Fortran
import Extensions.UnitsEnvironment

-- Top-level, select the solver
solveSystem :: (?solver :: Solver) => LinearSystem -> Consistency LinearSystem
solveSystem = case ?solver of
                  LAPACK -> solveSystemL
                  Custom -> solveSystemC
                  
-- Top-level custom solver
solveSystemC :: LinearSystem -> Consistency LinearSystem
solveSystemC system = solveSystem' system 1 1
solveSystemL system@(m0, _) = 
    let ((lhs, rhs), names) = convertToLapackFormat system
    in unsafePerformIO $
            do x <- catch (evaluate $ (linearSolveR lhs rhs))
                             (\e -> let x = (e :: SomeException) in return $ LM.fromLists [])
               if (LM.toLists x == []) 
                 then return $ BadL system 
                 else let rhs' = convertFromLapackFormat (x, names)
                          m = identity (ncols m0) 
                      in return $ Ok (m, rhs')

-- Converts the matrix into a format for LAPACK
convertToLapackFormat :: LinearSystem -> ((LM.Matrix Double, LM.Matrix Double), [MeasureUnit])
convertToLapackFormat (m, rhs) = 
    let r = nrows m
        c = ncols m
        extractNames rs (Unitless _) = rs
        extractNames rs (Unitful rw) = rs ++ (map fst rw)
        names = sort $ nub $ foldl extractNames [] rhs
        padding = take ((nrows m) - (length names) - 1) (repeat 0)
        rhs' = LM.fromLists $ map (\x -> convRowRhs x padding names) rhs
        padRows = if r < c then take (c - r) (repeat (take c (repeat 0))) else []
        padCols = if c < r then take (r - c) (repeat 0) else []
        m' = LM.fromLists $ (map (\x -> padCols ++ (map fromRational x)) (toLists m)) ++ padRows
    in ((m', rhs'), names)

convRowRhs (Unitless r) pad names = ((fromRational r) : (take (length names) (repeat 0))) ++ pad
convRowRhs (Unitful rws) pad names = 0 : (convVec names rws pad)

convVec [] rows pad     = pad
convVec (n : ns) rows pad = case (lookup n rows) of
                               Nothing -> 0 : convVec ns rows pad
                               Just r  -> (fromRational r) : convVec ns rows pad
 
convertFromLapackFormat :: (LM.Matrix Double, [MeasureUnit]) -> [UnitConstant] 
convertFromLapackFormat (rhs, names) =
    map (\(x : xs) -> case (convRow xs names) of 
                        [] -> Unitless (toRational x)
                        xs -> Unitful xs) (LM.toLists rhs)

convRow xs [] = []
convRow (0 : xs) (n : ns) = convRow xs ns
convRow (r : xs) (n : ns) = (n, toRational r) : convRow xs ns


solveSystem' :: LinearSystem -> Col -> Row -> Consistency LinearSystem
solveSystem' (matrix, vector) m k
  | m > ncols matrix = efmap (cutSystem k) $ checkSystem (matrix, vector) k
  | otherwise = elimRow (matrix, vector) n m k
                where n = find (\n -> matrix ! (n, m) /= 0) [k .. nrows matrix]

cutSystem :: Int -> LinearSystem -> LinearSystem
cutSystem k (matrix, vector) = (matrix', vector')
  where matrix' = submatrix 1 (k - 1) 1 (ncols matrix) matrix
        vector' = take (k - 1) vector

checkSystem :: LinearSystem -> Row -> Consistency LinearSystem
checkSystem (matrix, vector) k
  | k > nrows matrix = Ok (matrix, vector)
  | vector !! (k - 1) /= Unitful [] = let vars = V.toList $ getRow k matrix 
                                          bad = Bad (matrix, vector) (vector !! (k - 1), vars)
                                      in bad
  | otherwise = checkSystem (matrix, vector) (k + 1)

elimRow :: LinearSystem -> Maybe Row -> Col -> Row -> Consistency LinearSystem
elimRow system Nothing m k = solveSystem' system (m + 1) k
elimRow (matrix, vector) (Just n) m k = -- (show (m, k)) `D.trace`
 solveSystem' system' (m + 1) (k + 1)
  where matrix' = let s = matrix ! (n, m) in 
                    (if (k == n) then id else switchRows k n)
                       (if s == 1 then matrix else scaleRow (recip $ s) n matrix)
        vector' = switchScaleElems k n (fromRational $ recip $ matrix ! (n, m)) vector
        system' = elimRow' (matrix', vector') k m

elimRow' :: LinearSystem -> Row -> Col -> LinearSystem
elimRow' (matrix, vector) k m = (matrix', vector')
  where mstep matrix n = let s = (- matrix ! (n, m)) in if s == 0 then matrix else combineRows n s k matrix 
        matrix' = foldl mstep matrix $ [1 .. k - 1] ++ [k + 1 .. nrows matrix]
        vector'' = [x - fromRational (matrix ! (n, m)) * vector !! (k - 1) | (n, x) <- zip [1..] vector]
        (a, _ : b) = splitAt (k - 1) vector''
        vector' = a ++ vector !! (k - 1) : b

switchScaleElems :: Num a => Int -> Int -> a -> [a] -> [a]
switchScaleElems i j factor list = a ++ factor * b : c
  where (lj, b:rj) = splitAt (j - 1) list
        (a, _:c) = splitAt (i - 1) (lj ++ list !! (i - 1) : rj)
