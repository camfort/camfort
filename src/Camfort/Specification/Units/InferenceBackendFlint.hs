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
  ( atIndex, (<>)
  , rank, (?)
  , rows, cols
  , subMatrix, diag
  , fromBlocks, ident
  )
import qualified Numeric.LinearAlgebra as H

import Control.Monad

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Debug.Trace (trace, traceM, traceShowM)

foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_init" fmpz_mat_init :: Ptr FMPZMat -> CLong -> CLong -> IO ()
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_entry" fmpz_mat_entry :: Ptr FMPZMat -> CLong -> CLong -> IO (Ptr CLong)
foreign import ccall unsafe "flint/fmpz.h fmpz_set_si" fmpz_set_si :: Ptr CLong -> CLong -> IO ()
foreign import ccall unsafe "flint/fmpz.h fmpz_get_si" fmpz_get_si :: Ptr CLong -> IO CLong
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_clear" fmpz_mat_clear :: Ptr FMPZMat -> IO ()
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_print_pretty" fmpz_mat_print_pretty :: Ptr FMPZMat -> IO ()
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_mul" fmpz_mat_mul :: Ptr FMPZMat -> Ptr FMPZMat -> Ptr FMPZMat -> IO ()

-- r <- fmp_mat_rref B den A
--
-- Uses fraction-free Gauss-Jordan elimination to set (B, den) to the
-- reduced row echelon form of A and returns the rank of A. Aliasing
-- of A and B is allowed. r is rank of A.
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_rref" fmpz_mat_rref :: Ptr FMPZMat -> Ptr CLong -> Ptr FMPZMat -> IO CLong

-- fmpz_mat_hnf H A
--
-- H is the Hermite Normal Form of A.
foreign import ccall unsafe "flint/fmpz_mat.h fmpz_mat_hnf" fmpz_mat_hnf :: Ptr FMPZMat -> Ptr FMPZMat -> IO ()



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
      r <- fmpz_mat_hnf outputM inputM

      -- DEBUG:
      fmpz_mat_print_pretty outputM
      --

      lists <- forM [0..numRows-1] $ \ i -> do
        forM [0..numCols-1] $ \ j -> do
          e <- fmpz_mat_entry outputM i j
          fromIntegral `fmap` fmpz_get_si e
      let m' = H.fromLists lists
      fmpz_mat_clear inputM
      fmpz_mat_clear outputM
      return m'
