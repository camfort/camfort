{-# LANGUAGE ImplicitParams, BangPatterns #-}

module Extensions.UnitsSolve where

import Data.List
import Data.Matrix
import qualified Data.Vector as V
import Numeric.LinearAlgebra.LAPACK
import qualified Data.Packed.Matrix as LM
import Control.Exception
import System.IO.Unsafe
import qualified Debug.Trace as D


import Language.Fortran
import Extensions.UnitsEnvironment

-- Top-level, select the solver
solveSystem :: (?solver :: Solver) => LinearSystem -> Consistency LinearSystem
solveSystem = case ?solver of
                  LAPACK -> solveSystemL
                  Custom -> solveSystemC
                  
------------------------------------
-- LAPACK Solver
------------------------------------

solveSystemL :: LinearSystem -> Consistency LinearSystem
solveSystemL system@(m0, _) = 
    let ((!lhs, !rhs), !names) = convertToLapackFormat system
        x = unsafePerformIO $ catch (evaluate $ (linearSolveSVDR Nothing lhs rhs))
                                        (\e -> let x = (e :: SomeException) in return $ LM.fromLists [])
    in  if (LM.toLists x == []) 
                 then ("FAILO" ++ (show (lhs, rhs))) `D.trace` BadL system 
                 else ("Solved = " ++ show x) `D.trace`  
                     let rhs' = convertFromLapackFormat (x, names)
                         rhs'' = take (nrows m0) rhs'
                         m = extendTo 0 (nrows m0) (ncols m0) (identity ((nrows m0) `min` (ncols m0)))
                     in Ok (m, rhs'')

-- Converts the matrix into a format for LAPACK
convertToLapackFormat :: LinearSystem -> ((LM.Matrix Double, LM.Matrix Double), [MeasureUnit])
convertToLapackFormat (m, rhs) = "convertTo" `D.trace` 
    let r = nrows m
        c = ncols m
        extractNames rs (UnitlessC _) = rs
        extractNames rs (Unitful rw) = rs ++ (map fst rw)
        names = "" : (sort $ nub $ foldl extractNames [] rhs)
        n = (r `max` c) `max` (length names + 1)
        padColsRhs = [] -- take (n - (length names + 1)) (repeat 0)
        padRowsRhs = [] -- take (n - r) (repeat (take n (repeat 0)))
        rhsData = (map (\x -> convRowRhs x padColsRhs names) (rhs)) ++ padRowsRhs
        rhs' = (LM.fromLists rhsData) -- :: LM.Matrix Double
        padRows = [] -- take (n - r) (repeat (1 : take (n - 1) (repeat 0))) 
        padCols = [] -- take (n - c) (repeat 0) 
        mdata = (map (\x -> padCols ++ (map fromRational x)) (toLists m)) ++ padRows
        m' = (LM.fromLists mdata) -- :: LM.Matrix Double
    in  (("m = " ++ show m' ++ "\nrhs = " ++ show rhs' ++ "\nnames = " ++ show names) 
               `D.trace` -- (error "out")
                ((m', rhs'), names))

convRowRhs (UnitlessC r) pad names = ((fromRational r) : (take ((length names)  - 1) (repeat 0))) ++ pad
convRowRhs (Unitful rws) pad names = (convVec names rws pad)

convVec [] rows pad     = pad
convVec (n : ns) rows pad = case (lookup n rows) of
                               Nothing -> 0 : convVec ns rows pad
                               Just r  -> (fromRational r) : convVec ns rows pad

toNearestEps :: RealFrac a => a -> a
toNearestEps x = (fromInteger $ round $ x * (10^n)) / (10.0^^n) where n = 5 :: Int
 
convertFromLapackFormat :: (LM.Matrix Double, [MeasureUnit]) -> [UnitConstant] 
convertFromLapackFormat (rhs, names) = -- 0 : 
    map (\(x : xs) -> case (convRow xs (tail names)) of 
                        [] -> UnitlessC (toRational . toNearestEps $ x)
                        xs -> Unitful xs) (LM.toLists rhs)

convRow xs [] = []
convRow [] xs = []
convRow (0 : xs) (n : ns) = convRow xs ns
convRow (r : xs) (n : ns) = (n, toRational . toNearestEps $ r) : convRow xs ns

--------------------------------------------------
-- CUSTOM SOLVER
--------------------------------------------------

-- Top-level custom solver
solveSystemC :: LinearSystem -> Consistency LinearSystem
solveSystemC system = solveSystem' system 1 1

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
                                          bad = Bad (matrix, vector) k (vector !! (k - 1), vars)
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
