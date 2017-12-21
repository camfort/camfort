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

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Camfort.Specification.Units.InferenceBackendFlint where

import System.IO.Unsafe (unsafePerformIO)
import Numeric.LinearAlgebra
  ( atIndex, (<>), (><)
  , rank, (?)
  , rows, cols
  , subMatrix, diag
  , fromBlocks, ident
  )
import qualified Numeric.LinearAlgebra as H

import Control.Monad

import Data.List (findIndex, partition, foldl')

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Debug.Trace (trace, traceM, traceShowM)

foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_init" fmpz_mat_init :: Ptr FMPZMat -> CLong -> CLong -> IO ()
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_set" fmpz_mat_set :: Ptr FMPZMat -> Ptr FMPZMat -> IO ()
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_entry" fmpz_mat_entry :: Ptr FMPZMat -> CLong -> CLong -> IO (Ptr CLong)
foreign import ccall unsafe "flint/fmpz.h fmpz_set_si" fmpz_set_si :: Ptr CLong -> CLong -> IO ()
foreign import ccall unsafe "flint/fmpz.h fmpz_get_si" fmpz_get_si :: Ptr CLong -> IO CLong
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_clear" fmpz_mat_clear :: Ptr FMPZMat -> IO ()
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_print_pretty" fmpz_mat_print_pretty :: Ptr FMPZMat -> IO ()
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_mul" fmpz_mat_mul :: Ptr FMPZMat -> Ptr FMPZMat -> Ptr FMPZMat -> IO ()

-- fmpz_mat_window_init(fmpz_mat_t window, const fmpz_mat_t mat, slong r1, slong c1, slong r2, slong c2)
--
-- Initializes the matrix window to be an r2 - r1 by c2 - c1 submatrix
-- of mat whose (0,0) entry is the (r1, c1) entry of mat. The memory
-- for the elements of window is shared with mat.
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_window_init" fmpz_mat_window_init :: Ptr FMPZMat -> Ptr FMPZMat -> CLong -> CLong -> CLong -> CLong -> IO ()

-- Frees the window (leaving underlying matrix alone).
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_window_clear" fmpz_mat_window_clear :: Ptr FMPZMat -> IO ()

-- r <- fmp_mat_rref B den A
--
-- Uses fraction-free Gauss-Jordan elimination to set (B, den) to the
-- reduced row echelon form of A and returns the rank of A. Aliasing
-- of A and B is allowed. r is rank of A.
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_rref" fmpz_mat_rref :: Ptr FMPZMat -> Ptr CLong -> Ptr FMPZMat -> IO CLong

-- r <- fmp_mat_inv B den A
--
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_inv" fmpz_mat_inv :: Ptr FMPZMat -> Ptr CLong -> Ptr FMPZMat -> IO CLong

-- fmpz_mat_hnf H A
--
-- H is the Hermite Normal Form of A.
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_hnf" fmpz_mat_hnf :: Ptr FMPZMat -> Ptr FMPZMat -> IO ()

foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_rank" fmpz_mat_rank :: Ptr FMPZMat -> IO CLong

data FMPZMat

instance Storable FMPZMat where
  sizeOf _ = 4 * sizeOf (undefined :: CLong)
  alignment _ = alignment (undefined :: CLong)
  peek _ = undefined
  poke _ = undefined

testFlint = do
  traceM "***********************8 testFlint 8***********************"
  alloca $ \ a -> do
    alloca $ \ b -> do
      alloca $ \ den -> do
        let n = 10
        fmpz_mat_init a n n
        fmpz_mat_init b n n
        forM_ [0..n-1] $ \ i -> do
          forM_ [0..n-1] $ \ j -> do
            e <- fmpz_mat_entry a i j
            fmpz_set_si e (2 * i + j)

        -- fmpz_mat_mul b a a
        r <- fmpz_mat_rref b den a
        fmpz_mat_print_pretty a
        fmpz_mat_print_pretty b
        d <- peek den
        traceM $ "r = " ++ show r ++ " den = " ++ show d
        fmpz_mat_hnf b a
        fmpz_mat_print_pretty b
        fmpz_mat_clear a
        fmpz_mat_clear b


rref :: H.Matrix Double -> (H.Matrix Double, Int, Int)
rref m = unsafePerformIO $ do
  alloca $ \ outputM -> do
    alloca $ \ inputM -> do
      alloca $ \ den -> do
        let numRows = fromIntegral $ rows m
        let numCols = fromIntegral $ cols m
        fmpz_mat_init outputM numRows numCols
        fmpz_mat_init inputM numRows numCols
        forM_ [0..numRows-1] $ \ i -> do
          forM_ [0..numCols-1] $ \ j -> do
            e <- fmpz_mat_entry inputM i j
            fmpz_set_si e (floor (m `atIndex` (fromIntegral i, fromIntegral j)))
        r <- fmpz_mat_rref outputM den inputM
        d <- peek den

        -- DEBUG:
        -- fmpz_mat_print_pretty outputM
        -- traceM $ "r = " ++ show r ++ " den = " ++ show d
        --

        lists <- forM [0..numRows-1] $ \ i -> do
          forM [0..numCols-1] $ \ j -> do
            e <- fmpz_mat_entry outputM i j
            fromIntegral `fmap` fmpz_get_si e
        let m' = H.fromLists lists
        fmpz_mat_clear inputM
        fmpz_mat_clear outputM
        return (m', fromIntegral d, fromIntegral r)

hnf :: H.Matrix Double -> H.Matrix Double
hnf m = unsafePerformIO $ do
  alloca $ \ outputM -> do
    alloca $ \ inputM -> do
      let numRows = fromIntegral $ rows m
      let numCols = fromIntegral $ cols m
      fmpz_mat_init outputM numRows numCols
      fmpz_mat_init inputM numRows numCols
      forM_ [0..numRows-1] $ \ i -> do
        forM_ [0..numCols-1] $ \ j -> do
          e <- fmpz_mat_entry inputM i j
          fmpz_set_si e (floor (m `atIndex` (fromIntegral i, fromIntegral j)))
      fmpz_mat_hnf outputM inputM
      r <- fmpz_mat_rank outputM

      -- DEBUG:
      -- fmpz_mat_print_pretty outputM
      -- traceM $ "rank = " ++ show r
      --

      lists <- forM [0..fromIntegral r-1] $ \ i -> do
        forM [0..numCols-1] $ \ j -> do
          e <- fmpz_mat_entry outputM i j
          fromIntegral `fmap` fmpz_get_si e
      let m' = H.fromLists lists
      fmpz_mat_clear inputM
      fmpz_mat_clear outputM
      let Just (m'', _) = inv m'
      return m'

inv :: H.Matrix Double -> Maybe (H.Matrix Double, Int)
inv m = unsafePerformIO $ do
  alloca $ \ outputM -> do
    alloca $ \ inputM -> do
      alloca $ \ den -> do
        let numRows = fromIntegral $ rows m
        let numCols = fromIntegral $ cols m
        fmpz_mat_init outputM numRows numCols
        fmpz_mat_init inputM numRows numCols
        forM_ [0..numRows-1] $ \ i -> do
          forM_ [0..numCols-1] $ \ j -> do
            e <- fmpz_mat_entry inputM i j
            fmpz_set_si e (floor (m `atIndex` (fromIntegral i, fromIntegral j)))
        r <- fmpz_mat_inv outputM den inputM
        d <- peek den

        -- DEBUG:
        fmpz_mat_print_pretty outputM
        traceM $ "r = " ++ show r ++ " den = " ++ show d
        --

        lists <- forM [0..numRows-1] $ \ i -> do
          forM [0..numCols-1] $ \ j -> do
            e <- fmpz_mat_entry outputM i j
            fromIntegral `fmap` fmpz_get_si e
        let m' = H.fromLists lists
        fmpz_mat_clear inputM
        fmpz_mat_clear outputM
        if r == 1 then
          return $ Just (m', fromIntegral d)
        else
          return Nothing

withMatrix :: H.Matrix Double -> ((CLong, CLong, Ptr FMPZMat) -> IO b) -> IO b
withMatrix m f = do
  alloca $ \ outputM -> do
    let numRows = fromIntegral $ rows m
    let numCols = fromIntegral $ cols m
    fmpz_mat_init outputM numRows numCols
    forM_ [0 .. numRows - 1] $ \ i ->
      forM_ [0 .. numCols - 1] $ \ j -> do
        e <- fmpz_mat_entry outputM i j
        fmpz_set_si e (floor (m `atIndex` (fromIntegral i, fromIntegral j)) :: CLong)
    x <- f (numRows, numCols, outputM)
    fmpz_mat_clear outputM
    return x

withBlankMatrix :: CLong -> CLong -> (Ptr FMPZMat -> IO b) -> IO b
withBlankMatrix numRows numCols f = do
  alloca $ \ outputM -> do
    fmpz_mat_init outputM (fromIntegral numRows) (fromIntegral numCols)
    x <- f outputM
    fmpz_mat_clear outputM
    return x

withWindow :: Ptr FMPZMat -> CLong -> CLong -> CLong -> CLong -> (Ptr FMPZMat -> IO b) -> IO b
withWindow underM r1 c1 r2 c2 f = do
  alloca $ \ window -> do
    fmpz_mat_window_init window underM r1 c2 r2 c2
    x <- f window
    fmpz_mat_window_clear window
    return x

flintToHMatrix numRows numCols flintM = do
  lists <- forM [0..numRows-1] $ \ i -> do
    forM [0..numCols-1] $ \ j -> do
      e <- fmpz_mat_entry flintM i j
      fromIntegral `fmap` fmpz_get_si e
  return $ H.fromLists lists

pokeM flintM i j v = do
  e <- fmpz_mat_entry flintM i j
  fmpz_set_si e v

peekM flintM i j = do
  e <- fmpz_mat_entry flintM i j
  fmpz_get_si e

copyMatrix m1 m2 r1 c1 r2 c2 =
  forM_ [r1 .. r2-1] $ \ i ->
    forM_ [c1 .. c2-1] $ \ j ->
      peekM m2 i j >>= pokeM m1 i j


-- 'windows' don't seem to work?
-- copyMatrix' m1 m2 r1 c1 r2 c2 =
--   withWindow m2 r1 c1 r2 c2 $ \ w2 ->
--     withWindow m1 r1 c1 r2 c2 $ \ w1 -> do
--       fmpz_mat_set w1 w2

--------------------------------------------------
-- 'normalising' Hermite Normal Form, for lack of a better name
--
-- The problem with HNF is that it will happily return a matrix where
-- the leading-coefficient diagonal contains integers > 1.
--
-- In some cases the leading-coefficient does not divide some of the
-- remaining numbers in the row.
--
-- This corresponds to the case where one of the solutions would be
-- fractional if we were dealing with rational matrices.
--
-- But we don't want fractional solutions. We need to bump the matrix
-- so that this situation does not arise.
--
-- This tends to happen due to implicit polymorphism.
--
-- We do this by adding another column that copies the column with the
-- problematic leading-coefficient.
--
-- We then set up a new row expressing the constraint that the old
-- column is equal to the new column, 1:1.
--
-- This prevents a 'fractional' solution.
--
-- Along the way we look for leading-coefficients > 1 that do divide
-- their entire row. Then we simply apply elementary row scaling to
-- the row so that the leading-coefficients is 1.
--
-- The result is a matrix where the leading-coefficients of all the
-- rows that matter are 1. It's not quite a 'reduced' form though
-- because it can be bigger than the original matrix.
--
-- As a result we also return a list of columns that were cloned this
-- way.
--
-- The resulting matrix can have non-zeroes above the
-- leading-coefficients in the same column, so I apply some elementary
-- row operations to fix that and bring things into RREF.
--
-- Running time is computation of Hermite Normal Form twice, plus
-- construction of a slightly larger matrix (possibly), plus scanning
-- through the matrix for leading-coefficients, and then again to
-- divide them out sometimes.

normHNF :: H.Matrix Double -> (H.Matrix Double, [Int])
normHNF m = (m', indices)
  where
    numCols = cols m
    indexLookup j | j < numCols = j
                  | otherwise = indices !! (j `mod` numCols)
    indices = map indexLookup $ concatMap snd rs1
    (rs1, (m',[]):rs2) = break (null . snd) results
    results = tail $ iterate (normHNF' . fst) (m, [])

normHNF' :: H.Matrix Double -> (H.Matrix Double, [Int])
normHNF' m = fmap (map fromIntegral) . unsafePerformIO $ withMatrix m normhnf

normhnf (numRows, numCols, inputM) = do
  withBlankMatrix numRows numCols $ \ outputM -> do
    fmpz_mat_hnf outputM inputM
    rank <- fmpz_mat_rank outputM
    -- scale the rows so that it is a RREF, returning the indices
    -- where leading-coefficients had to be scaled to 1.
    indices <- elemrowscale outputM rank numCols
    -- HNF allows non-zeroes above the leading-coefficients that
    -- were greater than 1, so also fix that to bring into RREF.
    elemrowadds outputM rank numCols indices

    -- column indices of leading co-efficients > 1
    lcoefs <- filter ((> 1) . head . snd) <$> forM [0 .. rank-1] (\ i -> do
      cs <- zip [0..] `fmap` sequence [ peekM outputM i j | j <- [0 .. numCols-1] ]
      let (_, (j, lcoef):rest) = span ((== 0) . snd) cs
      return ((i, j), lcoef:map snd rest))

    -- split the identified rows into two categories: those that can
    -- be divided out by the leading co-efficient, and those that
    -- cannot.
    let (multCands, consCands) = partition (\ (_, lcoef:rest) -> all ((== 0) . (`rem` lcoef)) rest) lcoefs

    -- apply elementary row scaling
    forM_ multCands $ \ ((i, j), lcoef:_) ->
      forM_ [j..numCols - 1] $ \ j' -> do
        x <- peekM outputM i j'
        pokeM outputM i j' $ x `div` lcoef

    -- identify columns that need additional constraints generated and
    -- their associated value d, which is the value of the non-leading
    -- co-efficient divided by the GCD of the row.
    let genColCons ((_, j), lcoef:rest) = (j, minNLcoef `div` gcd lcoef minNLcoef)
          where
            restABS   = map abs rest
            minNLcoef = minimum (filter (/= 0) restABS)

    let consCols = map genColCons consCands

    -- generate operations that poke 1.0 and -d into columns of the
    -- given row (matrix parameter supplied later); d is the value of
    -- the non-leading co-efficient that isn't divisible by the
    -- leading-coefficient, divided by the GCD of the row.
    let ops = [ \ flintM i -> do pokeM flintM i j 1
                                 pokeM flintM i (numCols + k) (-d)
              | ((j, d), k) <- zip consCols [0..] ]
    let numOps = fromIntegral (length ops)
    let numRows' = numOps + rank
    let numCols' = numOps + numCols

    withBlankMatrix numRows' numCols' $ \ outputM' -> do
      -- create larger matrix containing outputM as submatrix
      copyMatrix outputM' outputM 0 0 rank numCols

      -- apply the operations on the extra space to write the
      -- additional rows and columns providing the additional
      -- constraints
      forM (zip [rank..] ops) $ \ (i, op) -> op outputM' i

      -- re-run HNF
      withBlankMatrix numRows' numCols' $ \ outputM'' -> do
        fmpz_mat_hnf outputM'' outputM'
        rank' <- fmpz_mat_rank outputM''
        -- scale the rows so that it is a RREF, returning the indices
        -- where leading-coefficients had to be scaled to 1.
        indices <- elemrowscale outputM'' rank' numCols'
        -- HNF allows non-zeroes above the leading-coefficients that
        -- were greater than 1, so also fix that to bring into RREF.
        elemrowadds outputM'' rank' numCols' indices
        -- convert back to HMatrix form
        h <- flintToHMatrix rank' numCols' outputM''
        return (h, map fst consCols)

-- find leading-coefficients that are greater than 1 and scale those
-- rows accordingly to reach RREF
--
-- precondition: matrix outputM is in HNF
elemrowscale outputM rank numCols = do
  -- column indices of leading co-efficients > 1
  lcoefs <- filter ((> 1) . head . snd) <$> forM [0 .. rank-1] (\ i -> do
    cs <- zip [0..] `fmap` sequence [ peekM outputM i j | j <- [0 .. numCols-1] ]
    let (_, (j, lcoef):rest) = span ((== 0) . snd) cs
    return ((i, j), lcoef:map snd rest))

  let multCands = filter (\ (_, lcoef:rest) -> all ((== 0) . (`rem` lcoef)) rest) lcoefs

  -- apply elementary row scaling
  forM multCands $ \ ((i, j), lcoef:_) -> do
    forM_ [j..numCols - 1] $ \ j' -> do
      x <- peekM outputM i j'
      pokeM outputM i j' $ x `div` lcoef
    return (i, j)

-- use indices to guide elementary row additions to reach RREF
--
-- precondition: matrix outputM is in HNF save for work done by
-- elemrowscale, and indices is a list of coordinates where we have
-- just scaled the leading-coefficient to 1 and now we must look to
-- see if there are any non-zeroes in the column above the
-- leading-coefficient, because that is allowed by HNF.
elemrowadds outputM rank numCols indices = do
  -- look for non-zero members of the columns above the
  -- leading-coefficient and wipe them out.
  forM_ indices $ \ (lcI, lcJ) -> do
    let j = lcJ
    forM_ [0..lcI-1] $ \ i -> do
      -- (i, j) ranges over the elements of the column above leading
      -- co-efficient (lcI, lcJ).
      x <- peekM outputM i j
      if x == 0 then
        pure () -- nothing to do at row i
      else do
        -- (i, j) is non-zero and row i must be cancelled x times
        let sf = x -- scaling factor = non-zero magnitude
        forM_ [lcJ..numCols-1] $ \ j' -> do
          -- (i,   j') ranges over the row where we discovered a non-zero
          -- (lcI, j') ranges over the row with the leading co-efficient
          x1 <- peekM outputM i j'
          x2 <- peekM outputM lcI j'
          -- add lower row scaled by sf to upper row
          pokeM outputM i j' (x1 - x2 * sf)

--------------------------------------------------

m1 = (8><6)
 [ 1.0, 0.0, 0.0, -1.0,  0.0,  0.0
 , 0.0, 1.0, 0.0,  0.0, -1.0,  0.0
 , 0.0, 0.0, 1.0,  0.0,  0.0, -1.0
 , 1.0, 0.0, 0.0, -1.0,  0.0,  0.0
 , 0.0, 1.0, 0.0,  0.0, -1.0,  0.0
 , 0.0, 0.0, 1.0,  0.0,  0.0, -1.0
 , 0.0, 0.0, 0.0,  1.0, -4.0,  0.0
 , 0.0, 0.0, 0.0,  0.0,  4.0, -3.0 ] :: H.Matrix Double

m2 = (8><6)
 [ 1.0, 0.0, 0.0, -1.0,  0.0,  0.0
 , 0.0, 1.0, 0.0,  0.0, -1.0,  0.0
 , 0.0, 0.0, 1.0,  0.0,  0.0, -1.0
 , 1.0, 0.0, 0.0, -1.0,  0.0,  0.0
 , 0.0, 1.0, 0.0,  0.0, -1.0,  0.0
 , 0.0, 0.0, 1.0,  0.0,  0.0, -1.0
 , 0.0, 0.0, 0.0,  1.0, -6.0,  0.0
 , 0.0, 0.0, 0.0,  0.0,  6.0, -4.0 ] :: H.Matrix Double
