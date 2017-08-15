{-# LANGUAGE RankNTypes #-}
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
import           Data.Typeable                    ((:~:) (..), Typeable, gcast)

import           Data.Singletons

import           Language.Fortran.TypeModel.Basic
import           Language.Fortran.TypeModel.Singletons
import           Language.Fortran.TypeModel.SBV

--------------------------------------------------------------------------------
-- * Types that have D

class (SingI (ReprPrec a), SingI (ReprKind a), SymWord a) => HasRepr a where
  type ReprPrec a :: Precision
  type ReprKind a :: Kind

  dForType :: proxy a -> D (ReprPrec a) (ReprKind a) a

withRepr :: D p k a -> (HasRepr a => b) -> b
withRepr DInt8 x = x
withRepr DInt16 x = x
withRepr DInt32 x = x
withRepr DInt64 x = x
withRepr DFloat x = x
withRepr DDouble x = x
withRepr DBool8 x = x
withRepr DBool16 x = x
withRepr DBool32 x = x
withRepr DBool64 x = x
withRepr DChar x = x
withRepr DProp x = x

literalD :: D p k a -> a -> SBV a
literalD d = withRepr d literal

instance HasRepr Int8 where
  type ReprPrec Int8 = 'P8
  type ReprKind Int8 = 'KInt
  dForType _ = DInt8
instance HasRepr Int16 where
  type ReprPrec Int16 = 'P16
  type ReprKind Int16 = 'KInt
  dForType _ = DInt16
instance HasRepr Int32 where
  type ReprPrec Int32 = 'P32
  type ReprKind Int32 = 'KInt
  dForType _ = DInt32
instance HasRepr Int64 where
  type ReprPrec Int64 = 'P64
  type ReprKind Int64 = 'KInt
  dForType _ = DInt64

instance HasRepr Float where
  type ReprPrec Float = 'P32
  type ReprKind Float = 'KReal
  dForType _ = DFloat
instance HasRepr Double where
  type ReprPrec Double = 'P64
  type ReprKind Double = 'KReal
  dForType _ = DDouble

instance HasRepr Bool8 where
  type ReprPrec Bool8 = 'P8
  type ReprKind Bool8 = 'KLogical
  dForType _ = DBool8
instance HasRepr Bool16 where
  type ReprPrec Bool16 = 'P16
  type ReprKind Bool16 = 'KLogical
  dForType _ = DBool16
instance HasRepr Bool32 where
  type ReprPrec Bool32 = 'P32
  type ReprKind Bool32 = 'KLogical
  dForType _ = DBool32
instance HasRepr Bool64 where
  type ReprPrec Bool64 = 'P64
  type ReprKind Bool64 = 'KLogical
  dForType _ = DBool64

instance HasRepr Char8 where
  type ReprPrec Char8 = 'P8
  type ReprKind Char8 = 'KChar
  dForType _ = DChar

instance HasRepr Bool where
  type ReprPrec Bool = 'P64
  type ReprKind Bool = 'KProp
  dForType _ = DProp

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


precMax :: Sing p1 -> Sing p2 -> Sing (PrecMax p1 p2)
precMax p1 p2 = case (p1, p2) of
 (SP8  , SP8  ) -> sing
 (SP8  , SP16 ) -> sing
 (SP8  , SP32 ) -> sing
 (SP8  , SP64 ) -> sing
 (SP8  , SP128) -> sing
 (SP16 , SP16 ) -> sing
 (SP16 , SP32 ) -> sing
 (SP16 , SP64 ) -> sing
 (SP16 , SP128) -> sing
 (SP32 , SP32 ) -> sing
 (SP32 , SP64 ) -> sing
 (SP32 , SP128) -> sing
 (SP64 , SP64 ) -> sing
 (SP64 , SP128) -> sing
 (SP128, SP128) -> sing
 (SP16 , SP8  ) -> sing
 (SP32 , SP8  ) -> sing
 (SP64 , SP8  ) -> sing
 (SP128, SP8  ) -> sing
 (SP32 , SP16 ) -> sing
 (SP64 , SP16 ) -> sing
 (SP128, SP16 ) -> sing
 (SP64 , SP32 ) -> sing
 (SP128, SP32 ) -> sing
 (SP128, SP64 ) -> sing

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


numKindMax
  :: (Numeric k1, Numeric k2)
  => Sing k1 -> Sing k2 -> Sing (NumKindMax k1 k2)
numKindMax k1 k2 =
  case (k1, k2) of
    (SKInt, SKInt) -> sing
    (SKInt, SKReal) -> sing
    (SKReal, SKInt) -> sing
    (SKReal, SKReal) -> sing
    _ -> error "Impossible: numKindMax with non-numeric input kinds"

--------------------------------------------------------------------------------
-- * Matching on kinds

-- | The result of matching on the kind of a 'D'. Comes packaged with
-- constraints that all members of the matched kind satisfy.
data MatchKind k a where
  MKInt  :: (HasRepr a, SIntegral a, Integral a, OrdSymbolic (SBV a), UDiv (SBV a))
        => MatchKind 'KInt a
  MKReal :: (HasRepr a, IEEEFloating a, IEEEFloatConvertable a,
            OrdSymbolic (SBV a), UDiv (SBV a))
        => MatchKind 'KReal a
  MKLogical :: (HasRepr a, SymBool a) => MatchKind 'KLogical a
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

data DWithParams (p :: Precision) (k :: Kind) where
  DWithParams :: (Typeable a, HasRepr a) => D p k a -> DWithParams p k

data DWithType a where
  DWithType :: (SingI p, SingI k) => D p k a -> DWithType a


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
getDWithParams :: Sing p -> Sing k -> Maybe (DWithParams p k)
getDWithParams p k = case (p, k) of
  (SP8  , SKInt)     -> Just (DWithParams DInt8)
  (SP16 , SKInt)     -> Just (DWithParams DInt16)
  (SP32 , SKInt)     -> Just (DWithParams DInt32)
  (SP64 , SKInt)     -> Just (DWithParams DInt64)
  (SP32 , SKReal)    -> Just (DWithParams DFloat)
  (SP64 , SKReal)    -> Just (DWithParams DDouble)
  (SP8  , SKLogical) -> Just (DWithParams DBool8)
  (SP16 , SKLogical) -> Just (DWithParams DBool16)
  (SP32 , SKLogical) -> Just (DWithParams DBool32)
  (SP64 , SKLogical) -> Just (DWithParams DBool64)
  (SP8  , SKChar)    -> Just (DWithParams DChar)
  (SP64 , SKProp)    -> Just (DWithParams DProp)
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
  :: (SingI p1, SingI p2,
      SingI k1, SingI k2,
      Numeric k1, Numeric k2)
  => D p1 k1 a -> D p2 k2 b -> DWithParams (PrecMax p1 p2) (NumKindMax k1 k2)
maxD (_ :: D p1 k1 a) (_ :: D p2 k2 b) =
  case getDWithParams (precMax (sing :: Sing p1) (sing :: Sing p2))
                      (numKindMax (sing :: Sing k1) (sing :: Sing k2)) of
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

wrappingUdivMod
  :: WrappedSym a
  => (SBV (UnwrappedSym a) -> SBV (UnwrappedSym a) -> (SBV (UnwrappedSym a), SBV (UnwrappedSym a)))
  -> (SBV a -> SBV a -> (SBV a, SBV a))
wrappingUdivMod f x y =
  let (q, r) = f (unwrapSym x) (unwrapSym y)
  in (wrapSym q, wrapSym r)

instance UDiv (SBV Int8)  where udivMod = wrappingUdivMod sDivMod
instance UDiv (SBV Int16) where udivMod = wrappingUdivMod sDivMod
instance UDiv (SBV Int32) where udivMod = wrappingUdivMod sDivMod
instance UDiv (SBV Int64) where udivMod = wrappingUdivMod sDivMod

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
