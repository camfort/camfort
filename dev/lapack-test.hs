{-# LANGUAGE FlexibleInstances, ImplicitParams #-}

import Numeric.LinearAlgebra.LAPACK
import Numeric.LinearAlgebra.Algorithms
import Data.Packed.Matrix
import qualified Data.Matrix as M
import Extensions.Units
import Extensions.UnitsSolve
import Extensions.UnitsEnvironment
import Data.Ratio
import Data.List
import System.Random

import System.IO.Unsafe
import Control.Exception

import Debug.Trace
import Test.QuickCheck

coeff :: Num a => [[a]]
coeff = [[1, 0, 1, 0], [0, 4, 0, 3], [0, 0, 2, 0]]

rhs :: Num a => [a]
rhs = [1, 2, 3]

rhs' = [[0, 1, 0], [0, 2, 0], [0, 0, 3]]

{- Lapack -}
coeffL, rhsL :: Matrix Double
coeffL = fromLists $ coeff
rhsL = fromLists $ map (\x -> [x]) rhs -- (map (\x -> x) rhs)
solveL = linearSolveSVDR Nothing coeffL rhsL

{- CamFort -}
coeffC :: M.Matrix Rational
coeffC = M.fromLists coeff
rhsC = map (\x -> Unitless x) rhs
linearSys = (coeffC, rhsC)
solveC = let ?solver = Custom in solveSystem linearSys

data LSystem a = LSystem (M.Matrix a) [a] Int deriving (Show, Eq)

instance Random (Ratio Integer) where
    random g = let (n, g') = random g 
                   (d, g'') = random g' 
               in (n % d, g'')
    randomR (l, h) g = let (ln, hn) = (numerator l, numerator h)
                           (ld, hd) = (denominator l, denominator h)
                           (n, g') = randomR (ln, hn) g
                           (d, g'') = randomR (ld, hd) g'
                       in (n % d, g'')

instance (Arbitrary a, Num a, Random a) => Arbitrary (LSystem a) where
    -- NB: for square matrices, but row echelon form
    arbitrary = sized (\n -> do m <- choose(1, n)
                                xs <- vectorOf (n*m) (choose (1, 20)) -- arbitrary
                                v <- vectorOf n (choose (1, 20)) -- arbitrary
                                let mx = M.matrix n m (\(i, j) -> if (i <= j) then xs !! ((i-1)*n + (j-1)) else 0)
                                return $ LSystem mx v n)

{-
speedTest = do let g = (resize 40 arbitrary)::(Gen (LSystem Rational))
               lapackSolve g
               camfortSolve g
-}

toLapack :: LSystem Rational -> (Matrix Double, Matrix Double)
toLapack (LSystem m v n) = (fromLists (map (map fromRational) (M.toLists $ m)), 
                            fromLists (map (\x -> [fromRational x]) v))
--                            fromLists (map (\x -> ((fromRational x) : (take (n - 1) (repeat 0)))) v))

fromLapack :: (Matrix Double, Matrix Double) -> [Double]
fromLapack (m, v) = map (\x -> head x) (toLists v)

fromLapackSol :: Matrix Double -> [Double]
fromLapackSol v = map (\x -> toNearestEps $ head x) (toLists v)

toCamfort :: LSystem Rational -> LinearSystem
toCamfort (LSystem m v n) = (m, map (\x -> Unitless x) v)

fromCamfort :: LinearSystem -> [Double]
fromCamfort (m, v) = map (\(Unitless r) -> fromRational r) v

fromCamfortSol :: Consistency LinearSystem -> TestResult
fromCamfortSol (Ok m) = Solved $ fromCamfort m
fromCamfortSol (Bad m _) = "Error" `trace` Solved $ fromCamfort m -- sometimes the partial result is similar

data TestResult = Solved [Double] | Unsolved deriving Show

lapackSolve x = unsafePerformIO $
                 do x <- catch (evaluate $ (uncurry linearSolveLSR) . toLapack $ x)
                               (\e -> let x = (e :: SomeException) in return $ fromLists [])
                    if (toLists x == []) 
                     then return Unsolved
                     else return $ Solved . fromLapackSol $ x
                        
--lapackSolve x = fromLapackSol . (uncurry linearSolveR) . toLapack $ x
camfortSolve = let ?solver = Custom in fromCamfortSol . solveSystem . toCamfort


class ApproxEq t where
   (~~) :: t -> t -> Bool
instance ApproxEq Double where
   x ~~ y = -- abs (x - y) <= 0.00001
            let x' = toNearestEps x
                y' = toNearestEps y
            in x' == y'
instance ApproxEq a => ApproxEq [a] where
   x ~~ y = and (zipWith (~~) x y)
instance ApproxEq TestResult where
   (Solved x) ~~ (Solved y) = x ~~ y
   Unsolved ~~ Unsolved     = True
   x ~~ y                   = False

convTest = quickCheck (\x -> ((fromLapack . toLapack) x) == ((fromCamfort . toCamfort) x))
solveTest = quickCheck (\x@(LSystem _ _ n) -> if n >= 1
                                              then let l = lapackSolve x
                                                       c = camfortSolve x
                                                   in l ~~ c
                                              else True)

foo = LSystem (M.fromLists [[15 % 1, 17 % 1], 
                            [0 % 1,  8 % 1],
                            [0 % 1,  0 % 1]])  [4 % 1, 5 % 1, 10 % 1] 3


foo2 = LSystem (M.fromLists [[10 % 1, 2 % 1], 
                             [0 % 1,  0 % 1]])
                [(-8) % 1,(-5) % 1] 2

fooExample = LSystem (M.fromLists (map (map toRational) m)) rhs 10
               where
                 m = [[ 1.0, 0.0, 0.0,  0.0, 0.0,  0.0, 0.0,  0.0,  0.0,  0.0 ]
                    , [ 0.0, 0.0, 0.0,  0.0, 1.0,  0.0, 0.0,  0.0,  0.0,  0.0 ]
                    , [ 0.0, 1.0,  0.0, 0.0,  0.0, 1.0, -1.0,  0.0,  0.0, 0.0 ]
                    , [ 0.0, 0.0,  0.0, 0.0, -1.0, 0.0,  1.0,  0.0,  0.0, 0.0 ]
                    , [ 1.0, 0.0,  0.0, 0.0,  0.0, 0.0,  0.0, -1.0,  0.0, 0.0 ]
                    , [ 1.0, 0.0,  0.0, 0.0,  0.0, 0.0,  0.0,  0.0, -1.0, 0.0 ]
                    , [ 0.0, 0.0, -1.0, 0.0,  0.0, 0.0,  0.0,  0.0,  0.0, 1.0 ]]
                 rhs = [ 0.0, 1.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 ]

fooExample2 = linearSolveSVDR Nothing (fromLists m) (fromLists rhs)
     where 
       m = [[ 1.0,  0.0,  0.0,  0.0,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0.0,  0.0] 
           ,[ 0.0,  0.0,  0.0,  1.0,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0.0,  0.0]
           ,[ 0.0,  0.0,  0.0,  0.0,  1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0.0,  0.0]
           ,[ 0.0,  0.0,  0.0, -1.0,  0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0.0,  0.0]
           ,[ 0.0,  0.0,  0.0,  0.0, -1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0.0,  0.0]
           ,[ 0.0, -1.0,  0.0,  0.0,  0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,  0.0,  0.0]
           ,[ 0.0,  0.0, -1.0,  0.0,  0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,  0.0,  0.0]
           ,[ 0.0,  0.0,  0.0,  0.0,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0,  0.0, -1.0]
           ,[0.0,  0.0,  0.0,  0.0,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0,  1.0 ]]
       rhs = [[ 0.0, 0.0, 0.0]
             ,[0.0, 1.0, 0.0]
             ,[0.0, 0.0, 1.0]
             ,[0.0, 0.0, 0.0]
             ,[0.0, 0.0, 0.0]
             ,[0.0, 0.0, 0.0]
             ,[0.0, 0.0, 0.0]
             ,[0.0, 0.0, 0.0]
             ,[0.0, 0.0, 0.0 ]]