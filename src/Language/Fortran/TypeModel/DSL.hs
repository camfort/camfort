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

lit :: HasRepr a => a -> FExpr v a
lit x = EOp (OpLit (dForType [x]) x)

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


class (HasRepr a, Numeric (ReprKind a)) => Numops1 a where
  numop1 :: Op1 'OpNum -> t a -> FortranOp t a
  numop1 op x = Op1 op Op1Num (dForType x) (dForType x) x

class (HasRepr a, ReprKind a ~ 'KLogical) => Logicalops1 a where
  logicalop1 :: Op1 'OpLogical -> t a -> FortranOp t a
  logicalop1 op x = Op1 op Op1Logical (dForType x) (dForType x) x

  propop1 :: Op1 'OpProp -> t a -> FortranOp t Bool
  propop1 op x = Op1 op Op1Prop (dForType x) DProp x

instance (HasRepr a, Numeric (ReprKind a)) => Numops1 a
instance (HasRepr a, ReprKind a ~ 'KLogical) => Logicalops1 a


class (HasRepr a, HasRepr b, HasRepr c,
       Numeric (ReprKind a),
       Numeric (ReprKind b),
       ReprKind c ~ NumKindMax (ReprKind a) (ReprKind b),
       ReprPrec c ~ PrecMax (ReprPrec a) (ReprPrec b)) =>
      Numops2 a b c | a b -> c where

  numop2 :: Op2 'OpNum -> t a -> t b -> FortranOp t c
  numop2 op x y = Op2 op Op2Num (dForType x) (dForType y) (dForType (Proxy :: Proxy c)) x y

class (HasRepr a, HasRepr b, HasRepr c,
       ReprKind c ~ 'KLogical,
       ReprPrec c ~ 'P8,
       Comparable (ReprKind a) (ReprKind b)) =>
      Eqops2 a b c | a b -> c where

  eqop2 :: Op2 'OpEquality -> t a -> t b -> FortranOp t c
  eqop2 op x y = Op2 op Op2Eq (dForType x) (dForType y) (dForType (Proxy :: Proxy c)) x y

class (HasRepr a, HasRepr b, HasRepr c,
       ReprKind c ~ 'KLogical,
       ReprPrec c ~ 'P8,
       Comparable (ReprKind a) (ReprKind b)) =>
      Relops2 a b c | a b -> c where

  relop2 :: Op2 'OpRelational -> t a -> t b -> FortranOp t c
  relop2 op x y = Op2 op Op2Rel (dForType x) (dForType y) (dForType (Proxy :: Proxy c)) x y

class (HasRepr a, HasRepr b, HasRepr c,
       ReprKind a ~ 'KLogical,
       ReprKind b ~ 'KLogical,
       ReprKind c ~ 'KLogical,
       ReprPrec c ~ PrecMax (ReprPrec a) (ReprPrec b)) =>
      Logicalops2 a b c | a b -> c where

  logicalop2 :: Op2 'OpLogical -> t a -> t b -> FortranOp t c
  logicalop2 op x y = Op2 op Op2Logical (dForType x) (dForType y) (dForType (Proxy :: Proxy c)) x y


instance (HasRepr a, HasRepr b, HasRepr c,
       Numeric (ReprKind a),
       Numeric (ReprKind b),
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
