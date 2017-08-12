{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wall #-}

{-|

Models the Fortran type system in Haskell types and provides an expression
operator for Fortran operations that can be evaluated to 'SBV' values.

-}
module Camfort.Specification.Hoare.Translate.Ops
  (
  -- * Fortran Operations
    FortranOp(..)
  -- * Model of Fortran Types
  , Precision(..)
  , Kind(..)
  , OpKind(..)
  , Op1(..)
  , Op2(..)
  -- * Exported Semantic Types
  , Int8
  , Int16
  , Int32
  , Int64
  , Char8(..)
  , Bool8(..)
  , Bool16(..)
  , Bool32(..)
  , Bool64(..)
  ) where

import Data.Int
import Data.Word
import Data.Data

import Data.SBV hiding (Kind, KReal)

import Language.Expression

-- TODO: Complex numbers

--------------------------------------------------------------------------------
--  Semantic type wrappers
--------------------------------------------------------------------------------

newtype Char8 = Char8 { char8Val :: Word8 }
  deriving (HasKind, Eq, Ord, Show, Num, Read, Data)

newtype Bool8 = Bool8 { bool8Val :: Int8 }
  deriving (HasKind, Eq, Ord, Show, Num, Read, Data)
newtype Bool16 = Bool16 { bool16Val :: Int16 }
  deriving (HasKind, Eq, Ord, Show, Num, Read, Data)
newtype Bool32 = Bool32 { bool32Val :: Int32 }
  deriving (HasKind, Eq, Ord, Show, Num, Read, Data)
newtype Bool64 = Bool64 { bool64Val :: Int64 }
  deriving (HasKind, Eq, Ord, Show, Num, Read, Data)

class SymBool a where
  toSBool :: SBV a -> SBool
  fromSBool :: SBool -> SBV a

numTrue :: Num a => a
numTrue = 1

numNot :: Num a => a -> a
numNot = negate

numAnd :: Num a => a -> a -> a
numAnd x y = signum (x * y)

instance SymWord Char8


instance SymWord Bool8

instance SymBool Bool8 where
  toSBool x = x .> 0
  fromSBool b = ite b true false

instance Boolean Bool8 where
  true = numTrue
  bnot = numNot
  (&&&) = numAnd

instance Boolean (SBV Bool8) where
  true = numTrue
  bnot = numNot
  (&&&) = numAnd


instance SymWord Bool16

instance SymBool Bool16 where
  toSBool x = x .> 0
  fromSBool b = ite b true false

instance Boolean Bool16 where
  true = numTrue
  bnot = numNot
  (&&&) = numAnd

instance Boolean (SBV Bool16) where
  true = numTrue
  bnot = numNot
  (&&&) = numAnd


instance SymWord Bool32

instance SymBool Bool32 where
  toSBool x = x .> 0
  fromSBool b = ite b true false

instance Boolean Bool32 where
  true = numTrue
  bnot = numNot
  (&&&) = numAnd

instance Boolean (SBV Bool32) where
  true = numTrue
  bnot = numNot
  (&&&) = numAnd


instance SymWord Bool64

instance SymBool Bool64 where
  toSBool x = x .> 0
  fromSBool b = ite b true false

instance Boolean Bool64 where
  true = numTrue
  bnot = numNot
  (&&&) = numAnd

instance Boolean (SBV Bool64) where
  true = numTrue
  bnot = numNot
  (&&&) = numAnd

--------------------------------------------------------------------------------
--  Model of Fortran Types
--------------------------------------------------------------------------------

data Precision
  = P8
  | P16
  | P32
  | P64
  | P128

data Kind
  = KReal
  | KInt
  | KLogical
  | KChar

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

--------------------------------------------------------------------------------
--  Type Machinery
--------------------------------------------------------------------------------

-- | Finds the maximum of two precision types.
type family PrecMax a b where
  PrecMax 'P8 'P8 = 'P8
  PrecMax 'P8 'P16 = 'P16
  PrecMax 'P8 'P32 = 'P32
  PrecMax 'P8 'P64 = 'P64
  PrecMax 'P8 'P128 = 'P128
  PrecMax 'P16 'P16 = 'P16
  PrecMax 'P16 'P32 = 'P32
  PrecMax 'P16 'P64 = 'P64
  PrecMax 'P16 'P128 = 'P128
  PrecMax 'P32 'P32 = 'P32
  PrecMax 'P32 'P64 = 'P64
  PrecMax 'P32 'P128 = 'P128
  PrecMax 'P64 'P64 = 'P64
  PrecMax 'P64 'P128 = 'P128
  PrecMax 'P128 'P128 = 'P128

  PrecMax 'P16 'P8 = 'P16
  PrecMax 'P32 'P8 = 'P32
  PrecMax 'P64 'P8 = 'P64
  PrecMax 'P128 'P8 = 'P128
  PrecMax 'P32 'P16 = 'P32
  PrecMax 'P64 'P16 = 'P64
  PrecMax 'P128 'P16 = 'P128
  PrecMax 'P64 'P32 = 'P64
  PrecMax 'P128 'P32 = 'P128
  PrecMax 'P128 'P64 = 'P128

-- | Finds the 'maximum' of two numeric kinds, where 'maximum' means the result
-- kind when a binary operation is applied values of the two input kinds.
type family NumKindMax k1 k2 where
  NumKindMax 'KInt 'KInt = 'KInt
  NumKindMax 'KInt 'KReal = 'KReal
  NumKindMax 'KReal 'KInt = 'KReal
  NumKindMax 'KReal 'KReal = 'KReal

-- | Lists the allowed Fortran types, with corresponding constraints on
-- precision, kind and semantic Haskell type.
data D (p :: Precision) (k :: Kind) a where
  DInt8          :: D 'P8   'KInt     Int8
  DInt16         :: D 'P16  'KInt     Int16
  DInt32         :: D 'P32  'KInt     Int32
  DInt64         :: D 'P64  'KInt     Int64

  DBool8         :: D 'P8   'KLogical Bool8
  DBool16        :: D 'P16  'KLogical Bool16
  DBool32        :: D 'P32  'KLogical Bool32
  DBool64        :: D 'P64  'KLogical Bool64

  DFloat         :: D 'P32  'KReal    Float
  DDouble        :: D 'P64  'KReal    Double

  DChar          :: D 'P8   'KChar    Char8
deriving instance Show (D p k a)
deriving instance Typeable (D p k a)

data SomeD p k where
  SomeD :: D p k a -> SomeD p k


-- | Maximises the D with respect to precision and kind.
maxD :: D p1 k1 a -> D p2 k2 b -> SomeD (PrecMax p1 p2) (NumKindMax k1 k2)
maxD DInt8   DInt8   = SomeD DInt8
maxD DInt8   DInt16  = SomeD DInt16
maxD DInt8   DInt32  = SomeD DInt32
maxD DInt8   DInt64  = SomeD DInt64
maxD DInt8   DFloat  = SomeD DFloat
maxD DInt8   DDouble = SomeD DDouble
maxD DInt16  DInt16  = SomeD DInt16
maxD DInt16  DInt32  = SomeD DInt32
maxD DInt16  DInt64  = SomeD DInt64
maxD DInt16  DFloat  = SomeD DFloat
maxD DInt16  DDouble = SomeD DDouble
maxD DInt32  DInt32  = SomeD DInt32
maxD DInt32  DInt64  = SomeD DInt64
maxD DInt32  DFloat  = SomeD DFloat
maxD DInt32  DDouble = SomeD DDouble
maxD DInt64  DInt64  = SomeD DInt64
maxD DInt64  DFloat  = SomeD DDouble
maxD DInt64  DDouble = SomeD DDouble
maxD DFloat  DFloat  = SomeD DFloat
maxD DFloat  DDouble = SomeD DDouble
maxD DDouble DDouble = SomeD DDouble
maxD DInt16  DInt8   = SomeD DInt16
maxD DInt32  DInt8   = SomeD DInt32
maxD DInt64  DInt8   = SomeD DInt64
maxD DFloat  DInt8   = SomeD DFloat
maxD DDouble DInt8   = SomeD DDouble
maxD DInt32  DInt16  = SomeD DInt32
maxD DInt64  DInt16  = SomeD DInt64
maxD DFloat  DInt16  = SomeD DFloat
maxD DDouble DInt16  = SomeD DDouble
maxD DInt64  DInt32  = SomeD DInt64
maxD DFloat  DInt32  = SomeD DFloat
maxD DDouble DInt32  = SomeD DDouble
maxD DFloat  DInt64  = SomeD DDouble
maxD DDouble DInt64  = SomeD DDouble
maxD DDouble DFloat  = SomeD DDouble
maxD _ _ = error "maxD on non-numeric kinds"


-- | Division operations with different behaviour for integral types compared to
-- real types.
class UDiv a where
  udivMod :: a -> a -> (a, a)

  udiv :: a -> a -> a
  udiv x y = fst (udivMod x y)

  umod :: a -> a -> a
  umod x y = snd (udivMod x y)

  default udivMod :: SDivisible a => a -> a -> (a, a)
  udivMod = sDivMod

instance UDiv (SBV Int8)
instance UDiv (SBV Int16)
instance UDiv (SBV Int32)
instance UDiv (SBV Int64)

instance UDiv (SBV Float) where
  udivMod x y = (udiv x y, umod x y)
  udiv = (/)
  umod = fpRem

instance UDiv (SBV Double) where
  udivMod x y = (udiv x y, umod x y)
  udiv = (/)
  umod = fpRem

-- | Reifies the idea that certain Fortran type kinds have corresponding semantic
-- types that always satisfy certain constraints.
data C k a where
  CInt  :: (SIntegral a, OrdSymbolic (SBV a), UDiv (SBV a))
        => C 'KInt a
  CReal :: (IEEEFloating a, IEEEFloatConvertable a,
            OrdSymbolic (SBV a), UDiv (SBV a))
        => C 'KReal a
  CLogical :: SymBool a => C 'KLogical a
  CChar :: C 'KChar Char8

getC :: D p k a -> C k a
getC DInt8 = CInt
getC DInt16 = CInt
getC DInt32 = CInt
getC DInt64 = CInt
getC DFloat = CReal
getC DDouble = CReal
getC DBool8 = CLogical
getC DBool16 = CLogical
getC DBool32 = CLogical
getC DBool64 = CLogical
getC DChar = CChar


data Op1Result ok k1 k2 where
  Op1NumInt :: Op1Result 'OpNum 'KInt 'KInt
  Op1NumReal :: Op1Result 'OpNum 'KReal 'KReal

  Op1LogicalLogical :: Op1Result 'OpLogical 'KLogical 'KLogical

deriving instance Show (Op1Result ok k1 k2)

type family Comparable k1 k2 where
  Comparable 'KInt  'KInt  = ()
  Comparable 'KInt  'KReal = ()
  Comparable 'KReal 'KInt  = ()
  Comparable 'KReal 'KReal = ()
  Comparable 'KChar 'KChar = ()


data Op2Result ok k1 p1 k2 p2 k3 p3 where
  Op2Num :: Op2Result 'OpNum k1 p1 k2 p2 (NumKindMax k1 k2) (PrecMax p1 p2)
  Op2Eq :: Comparable k1 k2 -> Op2Result 'OpEquality k1 p1 k2 p2 'KLogical 'P8
  Op2Rel :: Comparable k1 k2 -> Op2Result 'OpRelational k1 p1 k2 p2 'KLogical 'P8
  Op2Logical
    :: Op2Result 'OpLogical 'KLogical p1 'KLogical p2
                 'KLogical (PrecMax p1 p2)

-- deriving instance Show (Op2Result ok k1 p1 k2 p2 k3 p3)


data Op1C ok a b where
  Op1CNum :: (Num a, SymWord a, UDiv (SBV a)) => Op1C 'OpNum a a
  Op1CLogical :: SymBool a => Op1C 'OpLogical a a

op1C :: Op1Result ok k1 k2 -> D p1 k1 a -> D p2 k2 a -> Op1C ok a a
op1C Op1NumInt d1 d2 =
  case (getC d1, getC d2) of
    (CInt, CInt) -> Op1CNum

op1C Op1NumReal d1 d2 =
  case (getC d1, getC d2) of
    (CReal, CReal) -> Op1CNum

op1C Op1LogicalLogical d1 d2 =
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
    (CInt, CInt, CInt) -> uncurry Op2CNum (numConvert d1 d2 d3)
    (CInt, CReal, CReal) -> uncurry Op2CNum (numConvert d1 d2 d3)
    (CReal, CInt, CReal) -> uncurry Op2CNum (numConvert d1 d2 d3)
    (CReal, CReal, CReal) -> uncurry Op2CNum (numConvert d1 d2 d3)
    _ -> error "impossible"

op2C (Op2Eq _) d1 d2 d3 =
  case (getC d1, getC d2, getC d3) of
    (CInt, CInt, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CInt -> uncurry Op2CEq (numConvert d1 d2 d4)
    (CInt, CReal, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CEq (numConvert d1 d2 d4)
    (CReal, CInt, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CEq (numConvert d1 d2 d4)
    (CReal, CReal, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CEq (numConvert d1 d2 d4)
    (CChar, CChar, CLogical) -> Op2CEq id id
    _ -> error "impossible"

op2C (Op2Rel _) d1 d2 d3 =
  case (getC d1, getC d2, getC d3) of
    (CInt, CInt, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CInt -> uncurry Op2CRel (numConvert d1 d2 d4)
    (CInt, CReal, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CRel (numConvert d1 d2 d4)
    (CReal, CInt, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CRel (numConvert d1 d2 d4)
    (CReal, CReal, CLogical) ->
      case maxD d1 d2 of
        SomeD d4 -> case getC d4 of
          CReal -> uncurry Op2CRel (numConvert d1 d2 d4)
    (CChar, CChar, CLogical) -> Op2CRel id id
    _ -> error "impossible"

op2C Op2Logical d1 d2 d3 =
  case (getC d1, getC d2, getC d3) of
    (CLogical, CLogical, CLogical) ->
      Op2CLogical (fromSBool . toSBool) (fromSBool . toSBool)

type Convert a b = SBV a -> SBV b

numConvert
  :: D p1 k1 a -> D p2 k2 b -> D (PrecMax p1 p2) (NumKindMax k1 k2) c
  -> (Convert a c, Convert b c)
numConvert DInt8 DInt8 DInt8 = (id, id)
numConvert DInt8 DInt16 DInt16 = (sFromIntegral, id)
numConvert DInt8 DInt32 DInt32 = (sFromIntegral, id)
numConvert DInt8 DInt64 DInt64 = (sFromIntegral, id)
numConvert DInt16 DInt16 DInt16 = (id, id)
numConvert DInt16 DInt32 DInt32 = (sFromIntegral, id)
numConvert DInt16 DInt64 DInt64 = (sFromIntegral, id)
numConvert DInt32 DInt32 DInt32 = (id, id)
numConvert DInt32 DInt64 DInt64 = (sFromIntegral, id)
numConvert DInt64 DInt64 DInt64 = (id, id)
numConvert DInt16 DInt8 DInt16 = (id, sFromIntegral)
numConvert DInt32 DInt8 DInt32 = (id, sFromIntegral)
numConvert DInt64 DInt8 DInt64 = (id, sFromIntegral)
numConvert DInt32 DInt16 DInt32 = (id, sFromIntegral)
numConvert DInt64 DInt16 DInt64 = (id, sFromIntegral)
numConvert DInt64 DInt32 DInt64 = (id, sFromIntegral)
numConvert DInt8 DFloat DFloat = (sFromIntegral, id)
numConvert DInt16 DFloat DFloat = (sFromIntegral, id)
numConvert DInt32 DFloat DFloat = (sFromIntegral, id)
numConvert DInt64 DFloat DDouble = (sFromIntegral, toSDouble sRTZ)
numConvert DInt8 DDouble DDouble = (sFromIntegral, id)
numConvert DInt16 DDouble DDouble = (sFromIntegral, id)
numConvert DInt32 DDouble DDouble = (sFromIntegral, id)
numConvert DInt64 DDouble DDouble = (sFromIntegral, id)
numConvert DFloat DInt8 DFloat = (id, sFromIntegral)
numConvert DFloat DInt16 DFloat = (id, sFromIntegral)
numConvert DFloat DInt32 DFloat = (id, sFromIntegral)
numConvert DFloat DInt64 DDouble = (toSDouble sRTZ, sFromIntegral)
numConvert DDouble DInt8 DDouble = (id, sFromIntegral)
numConvert DDouble DInt16 DDouble = (id, sFromIntegral)
numConvert DDouble DInt32 DDouble = (id, sFromIntegral)
numConvert DDouble DInt64 DDouble = (id, sFromIntegral)
numConvert DFloat DFloat DFloat = (id, id)
numConvert DFloat DDouble DDouble = (toSDouble sRTZ, id)
numConvert DDouble DFloat DDouble = (id, toSDouble sRTZ)
numConvert DDouble DDouble DDouble = (id, id)
numConvert _ _ _ = error "impossible"

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
