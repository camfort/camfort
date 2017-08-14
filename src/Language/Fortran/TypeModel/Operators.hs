{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wall #-}

module Language.Fortran.TypeModel.Operators
  (
  -- * Model of Fortran Operators
    FortranOp(..)
  , OpKind(..)
  , Op1(..)
  , Op2(..)
  , Op1Result(..)
  , Op2Result(..)
  ) where

import           Data.SBV                             hiding (KReal, Kind)

import           Language.Expression

import           Language.Fortran.TypeModel.Basic
import           Language.Fortran.TypeModel.Machinery

--------------------------------------------------------------------------------
--  Model of Fortran operators
--------------------------------------------------------------------------------

data OpKind
  = OpNum
  | OpEquality
  | OpRelational
  | OpLogical
  deriving (Show)

data Op1 k where
  OpPos, OpNeg :: Op1 'OpNum
  OpNot :: Op1 'OpLogical

deriving instance Show (Op1 k)

data Op2 k where
  OpAdd, OpSub, OpMul, OpDiv :: Op2 'OpNum
  OpEq, OpNE :: Op2 'OpEquality
  OpLT, OpLE, OpGT, OpGE :: Op2 'OpRelational
  OpAnd, OpOr, OpEquiv, OpNotEquiv :: Op2 'OpLogical

deriving instance Show (Op2 k)

data Op1Result ok k1 k2 where
  Op1Num :: Numeric k => Op1Result 'OpNum k k

  Op1Logical :: Op1Result 'OpLogical 'KLogical 'KLogical

deriving instance Show (Op1Result ok k1 k2)

data Op2Result ok k1 p1 k2 p2 k3 p3 where
  Op2Num :: Op2Result 'OpNum k1 p1 k2 p2 (NumKindMax k1 k2) (PrecMax p1 p2)
  Op2Eq :: Comparable k1 k2 => Op2Result 'OpEquality k1 p1 k2 p2 'KLogical 'P8
  Op2Rel :: Comparable k1 k2 => Op2Result 'OpRelational k1 p1 k2 p2 'KLogical 'P8
  Op2Logical
    :: Op2Result 'OpLogical 'KLogical p1 'KLogical p2
                 'KLogical (PrecMax p1 p2)


data FortranOp t a where
  Op1 :: Op1 ok -> Op1Result ok k1 k2
      -> D p k1 a -> D p k2 a
      -> t a -> FortranOp t a
  Op2 :: Op2 ok -> Op2Result ok k1 p1 k2 p2 k3 p3
      -> D p1 k1 a -> D p2 k2 b -> D p3 k3 c
      -> t a -> t b -> FortranOp t c

instance Operator FortranOp where
  htraverseOp f = \case
    Op1 op opr d1 d2 x -> Op1 op opr d1 d2 <$> f x
    Op2 op opr d1 d2 d3 x y -> Op2 op opr d1 d2 d3 <$> f x <*> f y

instance (Applicative f) => EvalOp f SBV FortranOp where
  evalOp f = \case
    Op1 OpPos opr d1 d2 x -> withOp1Num (op1C opr d1 d2) id <$> f x
    Op1 OpNeg opr d1 d2 x -> withOp1Num (op1C opr d1 d2) negate <$> f x
    Op1 OpNot opr d1 d2 x -> withOp1Logical (op1C opr d1 d2) bnot <$> f x

    Op2 OpAdd opr d1 d2 d3 x y ->
      withOp2CNum (op2C opr d1 d2 d3) (+) <$> f x <*> f y
    Op2 OpSub opr d1 d2 d3 x y ->
      withOp2CNum (op2C opr d1 d2 d3) (-) <$> f x <*> f y
    Op2 OpMul opr d1 d2 d3 x y ->
      withOp2CNum (op2C opr d1 d2 d3) (*) <$> f x <*> f y
    Op2 OpDiv opr d1 d2 d3 x y ->
      withOp2CNum (op2C opr d1 d2 d3) udiv <$> f x <*> f y

    Op2 OpEq opr d1 d2 d3 x y ->
      withOp2CEq (op2C opr d1 d2 d3) (.==) <$> f x <*> f y
    Op2 OpNE opr d1 d2 d3 x y ->
      withOp2CEq (op2C opr d1 d2 d3) (./=) <$> f x <*> f y

    Op2 OpLT opr d1 d2 d3 x y ->
      withOp2CRel (op2C opr d1 d2 d3) (.<) <$> f x <*> f y
    Op2 OpGT opr d1 d2 d3 x y ->
      withOp2CRel (op2C opr d1 d2 d3) (.>) <$> f x <*> f y
    Op2 OpLE opr d1 d2 d3 x y ->
      withOp2CRel (op2C opr d1 d2 d3) (.<=) <$> f x <*> f y
    Op2 OpGE opr d1 d2 d3 x y ->
      withOp2CRel (op2C opr d1 d2 d3) (.>=) <$> f x <*> f y

    Op2 OpAnd opr d1 d2 d3 x y ->
      withOp2CLogical (op2C opr d1 d2 d3) (&&&) <$> f x <*> f y
    Op2 OpOr opr d1 d2 d3 x y ->
      withOp2CLogical (op2C opr d1 d2 d3) (|||) <$> f x <*> f y
    Op2 OpEquiv opr d1 d2 d3 x y ->
      withOp2CLogical (op2C opr d1 d2 d3) (<=>) <$> f x <*> f y
    Op2 OpNotEquiv opr d1 d2 d3 x y ->
      withOp2CLogical (op2C opr d1 d2 d3) (\a b -> bnot (a <=> b)) <$> f x <*> f y

--------------------------------------------------------------------------------
--  Constraint Machinery
--------------------------------------------------------------------------------

data Op1C ok a b where
  Op1CNum :: (Num a, SymWord a, UDiv (SBV a)) => Op1C 'OpNum a a
  Op1CLogical :: SymBool a => Op1C 'OpLogical a a

op1C :: Op1Result ok k1 k2 -> D p1 k1 a -> D p2 k2 a -> Op1C ok a a
op1C Op1Num d1 d2 =
  case (getC d1, getC d2) of
    (CInt, CInt) -> Op1CNum
    (CReal, CReal) -> Op1CNum
    _ -> error "impossible"

op1C Op1Logical d1 d2 =
  case (getC d1, getC d2) of
    (CLogical, CLogical) -> Op1CLogical

data Op2C ok a b c where
  Op2CNum :: (Num c, SymWord c, UDiv (SBV c))
          => Convert a c -> Convert b c -> Op2C 'OpNum a b c

  Op2CEq :: (SymBool c, EqSymbolic (SBV d))
         => Convert a d -> Convert b d -> Op2C 'OpEquality a b c

  Op2CRel :: (SymBool c, OrdSymbolic (SBV d))
         => Convert a d -> Convert b d -> Op2C 'OpRelational a b c

  Op2CLogical :: (SymBool c)
              => Convert a c -> Convert b c -> Op2C 'OpLogical a b c

op2C
  :: Op2Result ok k1 p1 k2 p2 k3 p3
  -> D p1 k1 a -> D p2 k2 b -> D p3 k3 c
  -> Op2C ok a b c
op2C Op2Num d1 d2 d3 =
  case (getC d1, getC d2, getC d3) of
    (CInt, CInt, CInt) -> uncurry Op2CNum (numUpcast d1 d2 d3)
    (CInt, CReal, CReal) -> uncurry Op2CNum (numUpcast d1 d2 d3)
    (CReal, CInt, CReal) -> uncurry Op2CNum (numUpcast d1 d2 d3)
    (CReal, CReal, CReal) -> uncurry Op2CNum (numUpcast d1 d2 d3)
    _ -> error "impossible"

op2C Op2Eq d1 d2 d3 =
  case (getC d1, getC d2, getC d3) of
    (CInt, CInt, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CInt -> uncurry Op2CEq (numUpcast d1 d2 d4)
    (CInt, CReal, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CEq (numUpcast d1 d2 d4)
    (CReal, CInt, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CEq (numUpcast d1 d2 d4)
    (CReal, CReal, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CEq (numUpcast d1 d2 d4)
    (CChar, CChar, CLogical) -> Op2CEq id id
    _ -> error "impossible"

op2C Op2Rel d1 d2 d3 =
  case (getC d1, getC d2, getC d3) of
    (CInt, CInt, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CInt -> uncurry Op2CRel (numUpcast d1 d2 d4)
    (CInt, CReal, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CRel (numUpcast d1 d2 d4)
    (CReal, CInt, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CRel (numUpcast d1 d2 d4)
    (CReal, CReal, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CRel (numUpcast d1 d2 d4)
    (CChar, CChar, CLogical) -> Op2CRel id id
    _ -> error "impossible"

op2C Op2Logical d1 d2 d3 =
  case (getC d1, getC d2, getC d3) of
    (CLogical, CLogical, CLogical) ->
      Op2CLogical (fromSBool . toSBool) (fromSBool . toSBool)

withOp1Num
  :: Op1C 'OpNum a a
  -> ((Num a, SymWord a) => SBV a -> SBV a)
  -> SBV a -> SBV a
withOp1Num Op1CNum f = f

withOp1Logical
  :: Op1C 'OpLogical a a
  -> (SBool -> SBool)
  -> SBV a -> SBV a
withOp1Logical Op1CLogical f = fromSBool . f . toSBool

withOp2CNum
  :: Op2C 'OpNum a b c
  -> ((Num c, SymWord c, UDiv (SBV c)) => SBV c -> SBV c -> SBV c)
  -> SBV a -> SBV b -> SBV c
withOp2CNum (Op2CNum ac bc) f x y = f (ac x) (bc y)

withOp2CEq
  :: Op2C 'OpEquality a b c
  -> (forall x. (EqSymbolic x) => x -> x -> SBool)
  -> SBV a -> SBV b -> SBV c
withOp2CEq (Op2CEq ad bd) f x y = fromSBool $ f (ad x) (bd y)

withOp2CRel
  :: Op2C 'OpRelational a b c
  -> (forall x. (OrdSymbolic x) => x -> x -> SBool)
  -> SBV a -> SBV b -> SBV c
withOp2CRel (Op2CRel ad bd) f x y = fromSBool $ f (ad x) (bd y)

withOp2CLogical
  :: Op2C 'OpLogical a b c
  -> (SBool -> SBool -> SBool)
  -> SBV a -> SBV b -> SBV c
withOp2CLogical (Op2CLogical ac bc) f x y =
  fromSBool $ f (toSBool $ ac x) (toSBool $ bc y)
