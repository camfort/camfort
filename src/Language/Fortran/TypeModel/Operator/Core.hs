{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wall      #-}

-- TODO: Function calls
module Language.Fortran.TypeModel.Operator.Core where

import           Data.Singletons.TypeLits

import           Data.Vinyl

import           Language.Fortran.TypeModel.Singletons
import           Language.Fortran.TypeModel.Types

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

data OpResult ok args result where
  -- TODO: non-primitive literals (initialization)
  ORLit
    :: Prim p k a
    -> a
    -> OpResult 'OKLit '[] (PrimS a)

  ORNum1
    :: NumericBasicType k1
    -> Prim p1 k1 a
    -> Prim p2 k2 b
    -> OpResult 'OKNum '[PrimS a] (PrimS b)

  ORNum2
    :: NumericBasicType k1 -> NumericBasicType k2
    -> Prim p1 k1 a -> Prim p2 k2 b
    -> Prim (PrecMax p1 p2) (BasicTypeMax k1 k2) c
    -> OpResult 'OKNum '[PrimS a, PrimS b] (PrimS c)

  ORLogical1
    :: Prim p1 'BTLogical a
    -> Prim 'P8 'BTLogical b
    -> OpResult 'OKLogical '[PrimS a] (PrimS b)

  ORLogical2
    :: Prim p1 'BTLogical a
    -> Prim p2 'BTLogical b
    -> Prim 'P8 'BTLogical c
    -> OpResult 'OKLogical '[PrimS a, PrimS b] (PrimS c)

  OREq
    :: ComparableBasicTypes k1 k2
    -> Prim p1 k1 a -> Prim p2 k2 b
    -> Prim 'P8 'BTLogical c
    -> OpResult 'OKEq '[PrimS a, PrimS b] (PrimS c)

  ORRel
    :: ComparableBasicTypes k1 k2
    -> Prim p1 k1 a -> Prim p2 k2 b
    -> Prim 'P8 'BTLogical c
    -> OpResult 'OKRel '[PrimS a, PrimS b] (PrimS c)

  ORLookup
    :: D (Array i v)
    -> OpResult 'OKLookup '[Array i v, i] v

  ORDeref
    :: RElem '(fname, a) fields i
    => D (Record rname fields)
    -> SSymbol fname
    -> OpResult 'OKDeref '[Record rname fields] a

  ORWriteArr
    :: D (Array i v)
    -> OpResult 'OKWriteArr '[Array i v, i, v] (Array i v)

  ORWriteData
    :: RElem '(fname, a) fields i
    => D (Record rname fields) -- ^ Record to write to
    -> SSymbol fname           -- ^ Field to write
    -> D a                     -- ^ New value
    -> OpResult 'OKWriteData '[Record rname fields, a] (Record rname fields)

--------------------------------------------------------------------------------
--  Specific Operators
--------------------------------------------------------------------------------

data Op n ok where
  OpLit      :: Op 0 'OKLit

  OpNeg      :: Op 1 'OKNum
  OpPos      :: Op 1 'OKNum
  OpAdd      :: Op 2 'OKNum
  OpSub      :: Op 2 'OKNum
  OpMul      :: Op 2 'OKNum
  OpDiv      :: Op 2 'OKNum

  OpEq       :: Op 2 'OKEq
  OpNE       :: Op 2 'OKEq

  OpLT       :: Op 2 'OKRel
  OpLE       :: Op 2 'OKRel
  OpGT       :: Op 2 'OKRel
  OpGE       :: Op 2 'OKRel

  OpNot      :: Op 1 'OKLogical
  OpAnd      :: Op 2 'OKLogical
  OpOr       :: Op 2 'OKLogical
  OpEquiv    :: Op 2 'OKLogical
  OpNotEquiv :: Op 2 'OKLogical

  OpLookup   :: Op 2 'OKLookup

  OpDeref    :: Op 1 'OKDeref

  OpWriteArr :: Op 3 'OKWriteArr
  OpWriteData :: Op 2 'OKWriteData
