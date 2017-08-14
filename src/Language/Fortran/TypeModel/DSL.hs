{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Language.Fortran.TypeModel.DSL where

import           Data.Typeable                        (Proxy (..))
import           Language.Expression

import           Language.Fortran.TypeModel.Basic
import           Language.Fortran.TypeModel.Machinery
import           Language.Fortran.TypeModel.Operators

type FExpr = Expr FortranOp

var :: v a -> FExpr v a
var = EVar

asProp :: Logicalops1 a => FExpr v a -> FExpr v Bool
asProp = EOp . propop1 OpEmbedProp

--------------------------------------------------------------------------------
--  Numeric
--------------------------------------------------------------------------------

neg :: Numops1 a => FExpr v a -> FExpr v a
neg = EOp . numop1 OpNeg

pos :: Numops1 a => FExpr v a -> FExpr v a
pos = EOp . numop1 OpPos

(.+) :: Numops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.+) = EOp ... numop2 OpAdd

(.-) :: Numops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.-) = EOp ... numop2 OpSub

(.*) :: Numops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.*) = EOp ... numop2 OpMul

(./) :: Numops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(./) = EOp ... numop2 OpDiv

--------------------------------------------------------------------------------
--  Equality
--------------------------------------------------------------------------------

(.==) :: Eqops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.==) = EOp ... eqop2 OpEq

(./=) :: Eqops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(./=) = EOp ... eqop2 OpNE

--------------------------------------------------------------------------------
--  Relational
--------------------------------------------------------------------------------

(.<) :: Relops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.<) = EOp ... relop2 OpLT

(.<=) :: Relops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.<=) = EOp ... relop2 OpLE

(.>) :: Relops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.>) = EOp ... relop2 OpGT

(.>=) :: Relops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.>=) = EOp ... relop2 OpGE

--------------------------------------------------------------------------------
--  Logical
--------------------------------------------------------------------------------

(.&&) :: Logicalops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.&&) = EOp ... logicalop2 OpAnd

(.||) :: Logicalops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.||) = EOp ... logicalop2 OpOr

(.<=>) :: Logicalops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.<=>) = EOp ... logicalop2 OpEquiv

(.</=>) :: Logicalops2 a b c => FExpr v a -> FExpr v b -> FExpr v c
(.</=>) = EOp ... logicalop2 OpNotEquiv

--------------------------------------------------------------------------------
--  Classes
--------------------------------------------------------------------------------

class HasRepr a where
  type ReprPrec a :: Precision
  type ReprKind a :: Kind

  getD :: proxy a -> D (ReprPrec a) (ReprKind a) a


instance HasRepr Int8 where
  type ReprPrec Int8 = 'P8
  type ReprKind Int8 = 'KInt
  getD _ = DInt8
instance HasRepr Int16 where
  type ReprPrec Int16 = 'P16
  type ReprKind Int16 = 'KInt
  getD _ = DInt16
instance HasRepr Int32 where
  type ReprPrec Int32 = 'P32
  type ReprKind Int32 = 'KInt
  getD _ = DInt32
instance HasRepr Int64 where
  type ReprPrec Int64 = 'P64
  type ReprKind Int64 = 'KInt
  getD _ = DInt64

instance HasRepr Float where
  type ReprPrec Float = 'P32
  type ReprKind Float = 'KReal
  getD _ = DFloat
instance HasRepr Double where
  type ReprPrec Double = 'P64
  type ReprKind Double = 'KReal
  getD _ = DDouble

instance HasRepr Bool8 where
  type ReprPrec Bool8 = 'P8
  type ReprKind Bool8 = 'KLogical
  getD _ = DBool8
instance HasRepr Bool16 where
  type ReprPrec Bool16 = 'P16
  type ReprKind Bool16 = 'KLogical
  getD _ = DBool16
instance HasRepr Bool32 where
  type ReprPrec Bool32 = 'P32
  type ReprKind Bool32 = 'KLogical
  getD _ = DBool32
instance HasRepr Bool64 where
  type ReprPrec Bool64 = 'P64
  type ReprKind Bool64 = 'KLogical
  getD _ = DBool64

instance HasRepr Char8 where
  type ReprPrec Char8 = 'P8
  type ReprKind Char8 = 'KChar
  getD _ = DChar


class (HasRepr a, Numeric (ReprKind a)) => Numops1 a where
  numop1 :: Op1 'OpNum -> t a -> FortranOp t a
  numop1 op x = Op1 op Op1Num (getD x) (getD x) x

class (HasRepr a, ReprKind a ~ 'KLogical) => Logicalops1 a where
  logicalop1 :: Op1 'OpLogical -> t a -> FortranOp t a
  logicalop1 op x = Op1 op Op1Logical (getD x) (getD x) x

  propop1 :: Op1 'OpProp -> t a -> FortranOp t Bool
  propop1 op x = Op1 op Op1Prop (getD x) DProp x

instance (HasRepr a, Numeric (ReprKind a)) => Numops1 a
instance (HasRepr a, ReprKind a ~ 'KLogical) => Logicalops1 a


class (HasRepr a, HasRepr b, HasRepr c,
       ReprKind c ~ NumKindMax (ReprKind a) (ReprKind b),
       ReprPrec c ~ PrecMax (ReprPrec a) (ReprPrec b)) =>
      Numops2 a b c | a b -> c where

  numop2 :: Op2 'OpNum -> t a -> t b -> FortranOp t c
  numop2 op x y = Op2 op Op2Num (getD x) (getD y) (getD (Proxy :: Proxy c)) x y

class (HasRepr a, HasRepr b, HasRepr c,
       ReprKind c ~ 'KLogical,
       ReprPrec c ~ 'P8,
       Comparable (ReprKind a) (ReprKind b)) =>
      Eqops2 a b c | a b -> c where

  eqop2 :: Op2 'OpEquality -> t a -> t b -> FortranOp t c
  eqop2 op x y = Op2 op Op2Eq (getD x) (getD y) (getD (Proxy :: Proxy c)) x y

class (HasRepr a, HasRepr b, HasRepr c,
       ReprKind c ~ 'KLogical,
       ReprPrec c ~ 'P8,
       Comparable (ReprKind a) (ReprKind b)) =>
      Relops2 a b c | a b -> c where

  relop2 :: Op2 'OpRelational -> t a -> t b -> FortranOp t c
  relop2 op x y = Op2 op Op2Rel (getD x) (getD y) (getD (Proxy :: Proxy c)) x y

class (HasRepr a, HasRepr b, HasRepr c,
       ReprKind a ~ 'KLogical,
       ReprKind b ~ 'KLogical,
       ReprKind c ~ 'KLogical,
       ReprPrec c ~ PrecMax (ReprPrec a) (ReprPrec b)) =>
      Logicalops2 a b c | a b -> c where

  logicalop2 :: Op2 'OpLogical -> t a -> t b -> FortranOp t c
  logicalop2 op x y = Op2 op Op2Logical (getD x) (getD y) (getD (Proxy :: Proxy c)) x y


instance (HasRepr a, HasRepr b, HasRepr c,
       ReprKind c ~ NumKindMax (ReprKind a) (ReprKind b),
       ReprPrec c ~ PrecMax (ReprPrec a) (ReprPrec b)) =>
      Numops2 a b c

instance (HasRepr a, HasRepr b, HasRepr c,
       ReprKind c ~ 'KLogical,
       ReprPrec c ~ 'P8,
       Comparable (ReprKind a) (ReprKind b)) =>
      Eqops2 a b c

instance (HasRepr a, HasRepr b, HasRepr c,
       ReprKind c ~ 'KLogical,
       ReprPrec c ~ 'P8,
       Comparable (ReprKind a) (ReprKind b)) =>
      Relops2 a b c

instance (HasRepr a, HasRepr b, HasRepr c,
       ReprKind a ~ 'KLogical,
       ReprKind b ~ 'KLogical,
       ReprKind c ~ 'KLogical,
       ReprPrec c ~ PrecMax (ReprPrec a) (ReprPrec b)) =>
      Logicalops2 a b c

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------

(f ... g) x y = f (g x y)
