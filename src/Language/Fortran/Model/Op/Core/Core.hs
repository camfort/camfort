{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE CPP                   #-}

{-# OPTIONS_GHC -Wall      #-}

-- TODO: Function calls
module Language.Fortran.Model.Op.Core.Core where

#if MIN_VERSION_singletons(3,0,0)
import           GHC.TypeLits.Singletons
#else
import           Data.Singletons.TypeLits
#endif

import           Data.Vinyl

import           Language.Fortran.Model.Singletons
import           Language.Fortran.Model.Types

--------------------------------------------------------------------------------
--  Closed Typeclasses on BasicTypes
--------------------------------------------------------------------------------

data NumericBasicType k where
  NBTInt :: NumericBasicType 'BTInt
  NBTReal :: NumericBasicType 'BTReal

data ComparableBasicTypes k1 k2 where
  CBTNum :: NumericBasicType k1 -> NumericBasicType k2 -> ComparableBasicTypes k1 k2
  CBTBool :: ComparableBasicTypes 'BTLogical 'BTLogical
  CBTChar :: ComparableBasicTypes 'BTChar 'BTChar

--------------------------------------------------------------------------------
--  Operator Result Types
--------------------------------------------------------------------------------

data OpSpec ok args result where
  -- TODO: non-primitive literals (initialization)
  OSLit
    :: Prim p k a
    -> a
    -> OpSpec 'OKLit '[] (PrimS a)

  OSNum1
    :: NumericBasicType k1
    -> Prim p1 k1 a
    -> Prim p2 k2 b
    -> OpSpec 'OKNum '[PrimS a] (PrimS b)

  OSNum2
    :: NumericBasicType k1 -> NumericBasicType k2
    -> Prim p1 k1 a -> Prim p2 k2 b
    -> Prim (PrecMax p1 p2) (BasicTypeMax k1 k2) c
    -> OpSpec 'OKNum '[PrimS a, PrimS b] (PrimS c)

  OSLogical1
    :: Prim p1 'BTLogical a
    -> Prim 'P8 'BTLogical b
    -> OpSpec 'OKLogical '[PrimS a] (PrimS b)

  OSLogical2
    :: Prim p1 'BTLogical a
    -> Prim p2 'BTLogical b
    -> Prim 'P8 'BTLogical c
    -> OpSpec 'OKLogical '[PrimS a, PrimS b] (PrimS c)

  OSEq
    :: ComparableBasicTypes k1 k2
    -> Prim p1 k1 a -> Prim p2 k2 b
    -> Prim 'P8 'BTLogical c
    -> OpSpec 'OKEq '[PrimS a, PrimS b] (PrimS c)

  OSRel
    :: ComparableBasicTypes k1 k2
    -> Prim p1 k1 a -> Prim p2 k2 b
    -> Prim 'P8 'BTLogical c
    -> OpSpec 'OKRel '[PrimS a, PrimS b] (PrimS c)

  OSLookup
    :: D (Array i v)
    -> OpSpec 'OKLookup '[Array i v, i] v

  OSDeref
    :: RElem '(fname, a) fields i
    => D (Record rname fields)
    -> SSymbol fname
    -> OpSpec 'OKDeref '[Record rname fields] a

--------------------------------------------------------------------------------
--  Specific Operators
--------------------------------------------------------------------------------

data Op n ok where
  OpLit       :: Op 0 'OKLit

  OpNeg       :: Op 1 'OKNum
  OpPos       :: Op 1 'OKNum

  OpAdd       :: Op 2 'OKNum
  OpSub       :: Op 2 'OKNum
  OpMul       :: Op 2 'OKNum
  OpDiv       :: Op 2 'OKNum

  OpEq        :: Op 2 'OKEq
  OpNE        :: Op 2 'OKEq

  OpLT        :: Op 2 'OKRel
  OpLE        :: Op 2 'OKRel
  OpGT        :: Op 2 'OKRel
  OpGE        :: Op 2 'OKRel

  OpNot       :: Op 1 'OKLogical
  OpAnd       :: Op 2 'OKLogical
  OpOr        :: Op 2 'OKLogical
  OpEquiv     :: Op 2 'OKLogical
  OpNotEquiv  :: Op 2 'OKLogical

  OpLookup    :: Op 2 'OKLookup

  OpDeref     :: Op 1 'OKDeref
