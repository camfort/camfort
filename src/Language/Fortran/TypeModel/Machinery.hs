{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wall #-}

module Language.Fortran.TypeModel.Machinery where

import           Data.Typeable ((:~:)(..))

import           Data.SBV                         hiding (KReal, Kind)

import           Language.Fortran.TypeModel.Basic

--------------------------------------------------------------------------------
-- * Type Families
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


class Comparable (k1 :: Kind) (k2 :: Kind)

instance Comparable 'KInt 'KInt
instance Comparable 'KInt 'KReal
instance Comparable 'KReal 'KInt
instance Comparable 'KReal 'KReal
instance Comparable 'KChar 'KChar


class Numeric (k :: Kind)
instance Numeric 'KInt
instance Numeric 'KReal

--------------------------------------------------------------------------------
-- * Existential type specs
--------------------------------------------------------------------------------

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
maxD _ _             = error "maxD on non-numeric kinds"


-- | Proof that two types with the same precision and kind have the same
-- representation.
dequiv :: D p k a -> D p k b -> a :~: b
dequiv DInt8 DInt8 = Refl
dequiv DInt16 DInt16 = Refl
dequiv DInt32 DInt32 = Refl
dequiv DInt64 DInt64 = Refl
dequiv DFloat DFloat = Refl
dequiv DDouble DDouble = Refl
dequiv DBool8 DBool8 = Refl
dequiv DBool16 DBool16 = Refl
dequiv DBool32 DBool32 = Refl
dequiv DBool64 DBool64 = Refl
dequiv DChar DChar = Refl
dequiv DProp DProp = Refl

--------------------------------------------------------------------------------
-- * Type Classes
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- * Constraints
--------------------------------------------------------------------------------

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

  CProp :: C 'KProp Bool

getC :: D p k a -> C k a
getC DInt8   = CInt
getC DInt16  = CInt
getC DInt32  = CInt
getC DInt64  = CInt
getC DFloat  = CReal
getC DDouble = CReal
getC DBool8  = CLogical
getC DBool16 = CLogical
getC DBool32 = CLogical
getC DBool64 = CLogical
getC DChar   = CChar
getC DProp   = CProp

--------------------------------------------------------------------------------
-- * Up-casting
--------------------------------------------------------------------------------

type Convert a b = SBV a -> SBV b

numUpcast
  :: D p1 k1 a -> D p2 k2 b -> D (PrecMax p1 p2) (NumKindMax k1 k2) c
  -> (Convert a c, Convert b c)
numUpcast DInt8 DInt8 DInt8       = (id, id)
numUpcast DInt8 DInt16 DInt16     = (sFromIntegral, id)
numUpcast DInt8 DInt32 DInt32     = (sFromIntegral, id)
numUpcast DInt8 DInt64 DInt64     = (sFromIntegral, id)
numUpcast DInt16 DInt16 DInt16    = (id, id)
numUpcast DInt16 DInt32 DInt32    = (sFromIntegral, id)
numUpcast DInt16 DInt64 DInt64    = (sFromIntegral, id)
numUpcast DInt32 DInt32 DInt32    = (id, id)
numUpcast DInt32 DInt64 DInt64    = (sFromIntegral, id)
numUpcast DInt64 DInt64 DInt64    = (id, id)
numUpcast DInt16 DInt8 DInt16     = (id, sFromIntegral)
numUpcast DInt32 DInt8 DInt32     = (id, sFromIntegral)
numUpcast DInt64 DInt8 DInt64     = (id, sFromIntegral)
numUpcast DInt32 DInt16 DInt32    = (id, sFromIntegral)
numUpcast DInt64 DInt16 DInt64    = (id, sFromIntegral)
numUpcast DInt64 DInt32 DInt64    = (id, sFromIntegral)
numUpcast DInt8 DFloat DFloat     = (sFromIntegral, id)
numUpcast DInt16 DFloat DFloat    = (sFromIntegral, id)
numUpcast DInt32 DFloat DFloat    = (sFromIntegral, id)
numUpcast DInt64 DFloat DDouble   = (sFromIntegral, toSDouble sRTZ)
numUpcast DInt8 DDouble DDouble   = (sFromIntegral, id)
numUpcast DInt16 DDouble DDouble  = (sFromIntegral, id)
numUpcast DInt32 DDouble DDouble  = (sFromIntegral, id)
numUpcast DInt64 DDouble DDouble  = (sFromIntegral, id)
numUpcast DFloat DInt8 DFloat     = (id, sFromIntegral)
numUpcast DFloat DInt16 DFloat    = (id, sFromIntegral)
numUpcast DFloat DInt32 DFloat    = (id, sFromIntegral)
numUpcast DFloat DInt64 DDouble   = (toSDouble sRTZ, sFromIntegral)
numUpcast DDouble DInt8 DDouble   = (id, sFromIntegral)
numUpcast DDouble DInt16 DDouble  = (id, sFromIntegral)
numUpcast DDouble DInt32 DDouble  = (id, sFromIntegral)
numUpcast DDouble DInt64 DDouble  = (id, sFromIntegral)
numUpcast DFloat DFloat DFloat    = (id, id)
numUpcast DFloat DDouble DDouble  = (toSDouble sRTZ, id)
numUpcast DDouble DFloat DDouble  = (id, toSDouble sRTZ)
numUpcast DDouble DDouble DDouble = (id, id)
numUpcast _ _ _                   =
  error "Can't upcast something that isn't a number"
