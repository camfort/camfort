{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall #-}

module Language.Fortran.TypeModel.Machinery where

import           Control.Applicative              (Alternative (..))
import           Data.Typeable                    ((:~:) (..), Proxy (..),
                                                   Typeable, gcast)

import           Data.SBV                         hiding (KReal, Kind)

import           Language.Fortran.TypeModel.Basic

--------------------------------------------------------------------------------
-- * Singletons for precisions and kinds

data SingPrec p where
  SingP8   :: SingPrec 'P8
  SingP16  :: SingPrec 'P16
  SingP32  :: SingPrec 'P32
  SingP64  :: SingPrec 'P64
  SingP128 :: SingPrec 'P128

class PrecSing p where
  precSing :: proxy p -> SingPrec p

instance PrecSing 'P8   where precSing _ = SingP8
instance PrecSing 'P16  where precSing _ = SingP16
instance PrecSing 'P32  where precSing _ = SingP32
instance PrecSing 'P64  where precSing _ = SingP64
instance PrecSing 'P128 where precSing _ = SingP128

data SingKind k where
  SingKInt     :: SingKind 'KInt
  SingKReal    :: SingKind 'KReal
  SingKLogical :: SingKind 'KLogical
  SingKChar    :: SingKind 'KChar
  SingKProp    :: SingKind 'KProp

class KindSing k where
  kindSing :: proxy k -> SingKind k

instance KindSing 'KInt     where kindSing _ = SingKInt
instance KindSing 'KReal    where kindSing _ = SingKReal
instance KindSing 'KLogical where kindSing _ = SingKLogical
instance KindSing 'KChar    where kindSing _ = SingKChar
instance KindSing 'KProp    where kindSing _ = SingKProp

--------------------------------------------------------------------------------
-- * Type Families

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


precMax :: (PrecSing p1, PrecSing p2) => proxy1 p1 -> proxy2 p2 -> SingPrec (PrecMax p1 p2)
precMax p1 p2 = case (precSing p1, precSing p2) of
 (SingP8  , SingP8  ) -> SingP8
 (SingP8  , SingP16 ) -> SingP16
 (SingP8  , SingP32 ) -> SingP32
 (SingP8  , SingP64 ) -> SingP64
 (SingP8  , SingP128) -> SingP128
 (SingP16 , SingP16 ) -> SingP16
 (SingP16 , SingP32 ) -> SingP32
 (SingP16 , SingP64 ) -> SingP64
 (SingP16 , SingP128) -> SingP128
 (SingP32 , SingP32 ) -> SingP32
 (SingP32 , SingP64 ) -> SingP64
 (SingP32 , SingP128) -> SingP128
 (SingP64 , SingP64 ) -> SingP64
 (SingP64 , SingP128) -> SingP128
 (SingP128, SingP128) -> SingP128
 (SingP16 , SingP8  ) -> SingP16
 (SingP32 , SingP8  ) -> SingP32
 (SingP64 , SingP8  ) -> SingP64
 (SingP128, SingP8  ) -> SingP128
 (SingP32 , SingP16 ) -> SingP32
 (SingP64 , SingP16 ) -> SingP64
 (SingP128, SingP16 ) -> SingP128
 (SingP64 , SingP32 ) -> SingP64
 (SingP128, SingP32 ) -> SingP128
 (SingP128, SingP64 ) -> SingP128


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


numKindMax :: (Numeric k1, Numeric k2, KindSing k1, KindSing k2) => proxy1 k1 -> proxy2 k2 -> SingKind (NumKindMax k1 k2)
numKindMax k1 k2 =
  case (kindSing k1, kindSing k2) of
    (SingKInt, SingKInt) -> SingKInt
    (SingKInt, SingKReal) -> SingKReal
    (SingKReal, SingKInt) -> SingKReal
    (SingKReal, SingKReal) -> SingKReal
    _ -> error "Impossible: numKindMax with non-numeric input kinds"

--------------------------------------------------------------------------------
-- * Matching on kinds

-- | The result of matching on the kind of a 'D'. Comes packaged with
-- constraints that all members of the matched kind satisfy.
data MatchKind k a where
  MKInt  :: (SIntegral a, Integral a, OrdSymbolic (SBV a), UDiv (SBV a))
        => MatchKind 'KInt a
  MKReal :: (IEEEFloating a, IEEEFloatConvertable a,
            OrdSymbolic (SBV a), UDiv (SBV a))
        => MatchKind 'KReal a
  MKLogical :: SymBool a => MatchKind 'KLogical a
  MKChar :: MatchKind 'KChar Char8

  MKProp :: MatchKind 'KProp Bool

matchKind :: D p k a -> MatchKind k a
matchKind DInt8   = MKInt
matchKind DInt16  = MKInt
matchKind DInt32  = MKInt
matchKind DInt64  = MKInt
matchKind DFloat  = MKReal
matchKind DDouble = MKReal
matchKind DBool8  = MKLogical
matchKind DBool16 = MKLogical
matchKind DBool32 = MKLogical
matchKind DBool64 = MKLogical
matchKind DChar   = MKChar
matchKind DProp   = MKProp

--------------------------------------------------------------------------------
-- * Existential D

data DWithParams p k where
  DWithParams :: Typeable a => D p k a -> DWithParams p k

data DWithType a where
  DWithType :: (PrecSing p, KindSing k) => D p k a -> DWithType a


-- | Two types with the same precision and kind have the same representation.
dequiv :: D p k a -> D p k b -> a :~: b
dequiv DInt8 DInt8     = Refl
dequiv DInt16 DInt16   = Refl
dequiv DInt32 DInt32   = Refl
dequiv DInt64 DInt64   = Refl
dequiv DFloat DFloat   = Refl
dequiv DDouble DDouble = Refl
dequiv DBool8 DBool8   = Refl
dequiv DBool16 DBool16 = Refl
dequiv DBool32 DBool32 = Refl
dequiv DBool64 DBool64 = Refl
dequiv DChar DChar     = Refl
dequiv DProp DProp     = Refl


-- | If the particular precision and kind are inhabited by a Fortran type, get
-- the type.
getDWithParams :: SingPrec p -> SingKind k -> Maybe (DWithParams p k)
getDWithParams p k = case (p, k) of
  (SingP8  , SingKInt)     -> Just (DWithParams DInt8)
  (SingP16 , SingKInt)     -> Just (DWithParams DInt16)
  (SingP32 , SingKInt)     -> Just (DWithParams DInt32)
  (SingP64 , SingKInt)     -> Just (DWithParams DInt64)
  (SingP32 , SingKReal)    -> Just (DWithParams DFloat)
  (SingP64 , SingKReal)    -> Just (DWithParams DDouble)
  (SingP8  , SingKLogical) -> Just (DWithParams DBool8)
  (SingP16 , SingKLogical) -> Just (DWithParams DBool16)
  (SingP32 , SingKLogical) -> Just (DWithParams DBool32)
  (SingP64 , SingKLogical) -> Just (DWithParams DBool64)
  (SingP8  , SingKChar)    -> Just (DWithParams DChar)
  (SingP64 , SingKProp)    -> Just (DWithParams DProp)
  _                        -> Nothing


getDWithType :: (Typeable a) => proxy1 a -> Maybe (DWithType a)
getDWithType _ =
  gcast (DWithType DInt8)   <|>
  gcast (DWithType DInt16)  <|>
  gcast (DWithType DInt32)  <|>
  gcast (DWithType DInt64)  <|>
  gcast (DWithType DFloat)  <|>
  gcast (DWithType DDouble) <|>
  gcast (DWithType DBool8)  <|>
  gcast (DWithType DBool16) <|>
  gcast (DWithType DBool32) <|>
  gcast (DWithType DBool64) <|>
  gcast (DWithType DChar)   <|>
  gcast (DWithType DProp)

-- | Maximises the D with respect to precision and kind.
maxD
  :: (PrecSing p1, PrecSing p2,
      KindSing k1, KindSing k2,
      Numeric k1, Numeric k2)
  => D p1 k1 a -> D p2 k2 b -> DWithParams (PrecMax p1 p2) (NumKindMax k1 k2)
maxD (_ :: D p1 k1 a) (_ :: D p2 k2 b) =
  let prec = precMax (Proxy :: Proxy p1) (Proxy :: Proxy p2)
      kind = numKindMax (Proxy :: Proxy k1) (Proxy :: Proxy k2)
  in case getDWithParams prec kind of
    Just d  -> d
    Nothing -> error "impossible: maxD result doesn't exist"

--------------------------------------------------------------------------------
-- * Type Classes

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
-- * Up-casting

type Convert a b = SBV a -> SBV b

realCast :: D p1 'KReal a -> D p2 'KReal b -> Convert a b
realCast DFloat DFloat   = id
realCast DFloat DDouble  = toSDouble sRTZ
realCast DDouble DFloat  = toSFloat sRTZ
realCast DDouble DDouble = id

numUpcast
  :: D p1 k1 a -> D p2 k2 b -> D (PrecMax p1 p2) (NumKindMax k1 k2) c
  -> (Convert a c, Convert b c)
numUpcast d1 d2 d3 =
  case (matchKind d1, matchKind d2, matchKind d3) of
    (MKInt, MKInt, MKInt) -> (sFromIntegral, sFromIntegral)
    (MKInt, MKReal, MKReal) -> (sFromIntegral, realCast d2 d3)
    (MKReal, MKInt, MKReal) -> (realCast d1 d3, sFromIntegral)
    (MKReal, MKReal, MKReal) -> (realCast d1 d3, realCast d2 d3)
    _ -> error "Impossible: numUpcast on non-numeric arguments"
