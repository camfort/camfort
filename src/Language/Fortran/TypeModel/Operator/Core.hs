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
--  Closed Typeclasses on Kinds
--------------------------------------------------------------------------------

data NumericKind k where
  NKInt :: NumericKind 'KInt
  NKReal :: NumericKind 'KReal

data ComparableKinds k1 k2 where
  CKNum :: NumericKind k1 -> NumericKind k2 -> ComparableKinds k1 k2
  CKBool :: ComparableKinds 'KLogical 'KLogical
  CKChar :: ComparableKinds 'KChar 'KChar

--------------------------------------------------------------------------------
--  Operator Result Types
--------------------------------------------------------------------------------

data OpResult ok args result where
  -- TODO: non-primitive literals (initialization)
  ORLit
    :: Prim p k (PrimS a)
    -> a
    -> OpResult 'OKLit '[] (PrimS a)

  ORNum1
    :: NumericKind k1
    -> Prim p1 k1 a
    -> Prim p2 k2 b
    -> OpResult 'OKNum '[a] b

  ORNum2
    :: NumericKind k1 -> NumericKind k2
    -> Prim p1 k1 a -> Prim p2 k2 b
    -> Prim (PrecMax p1 p2) (KindMax k1 k2) c
    -> OpResult 'OKNum '[a, b] c

  ORLogical1
    :: Prim p1 'KLogical a
    -> Prim 'P8 'KLogical b
    -> OpResult 'OKLogical '[a] b

  ORLogical2
    :: Prim p1 'KLogical a
    -> Prim p2 'KLogical b
    -> Prim 'P8 'KLogical c
    -> OpResult 'OKLogical '[a, b] c

  OREq
    :: ComparableKinds k1 k2
    -> Prim p1 k1 a -> Prim p2 k2 b
    -> Prim 'P8 'KLogical c
    -> OpResult 'OKEq '[a, b] c

  ORRel
    :: ComparableKinds k1 k2
    -> Prim p1 k1 a -> Prim p2 k2 b
    -> Prim 'P8 'KLogical c
    -> OpResult 'OKRel '[a, b] c

  ORLookup
    :: D (Array i v)
    -> OpResult 'OKLookup '[Array i v, i] v

  ORDeref
    :: RElem '(fname, a) fields i
    => D (Record rname fields)
    -> SSymbol fname
    -> OpResult 'OKDeref '[Record rname fields] a

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
