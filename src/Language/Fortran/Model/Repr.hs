{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wall #-}


{-|

Symbolic representations of Fortran values, for symbolic reasoning.

There is a distinction between core representations ('CoreRepr') and high-level
representations ('HighRepr'). 'CoreRepr' represents any @a@ such that @'D' a@
exists; i.e. anything with a corresponding Fortran type. 'HighRepr' is a
superset of 'CoreRepr'. It represents Fortran types, and also higher-level types
that facilitate reasoning. There is more information about this distinction in
"Language.Fortran.Model.Op".

-}
module Language.Fortran.Model.Repr where

import           Data.Int                        (Int16, Int32, Int64, Int8)
import           Data.Word                       (Word8)

import           Data.Functor.Compose

import           Data.SBV
import           Data.SBV.Dynamic
import           Data.SBV.Internals              (SBV (..))

import           Data.Vinyl                      hiding (Field)

import           Language.Expression
import           Language.Expression.Prop

import           Language.Fortran.Model.Repr.Prim
import           Language.Fortran.Model.Types

--------------------------------------------------------------------------------
-- * Core Fortran Representations

{-|

Symbolic representations of Fortran values, using "Data.SBV.Dynamic".

-}
data CoreRepr a where
  CRPrim
    :: D (PrimS a)
    -> SVal
    -> CoreRepr (PrimS a)

  CRArray
    :: D (Array i a)
    -> ArrRepr i a
    -> CoreRepr (Array i a)

  CRData
    :: D (Record name fs)
    -> Rec (Field CoreRepr) fs
    -> CoreRepr (Record name fs)

{-|

Symbolic respresentations of Fortran arrays. SBV arrays can only contain basic
values, so in order to represent arrays of derived data types, we use multiple
flat arrays, one for each basic field. Nested derived data types are recursively
expanded.

Arrays of arrays are not yet supported.

-}
data ArrRepr i a where
  -- | A primitive type is represented by a flat 'SArr'.
  ARPrim :: SArr -> ArrRepr i (PrimS a)

  -- | A derived data type is represented by a record of 'ArrRepr's over the
  -- record's fields.
  ARData :: Rec (Field (ArrRepr i)) fs -> ArrRepr i (Record name fs)


--------------------------------------------------------------------------------
-- * High-level data representations

{-|

Symbolic representations of Fortran values plus types in the higher-level
meta-language (see "Language.Fortran.Op" for more information).

-}
data HighRepr a where
  HRCore :: CoreRepr a -> HighRepr a
  HRHigh :: SBV a -> HighRepr a

instance HFoldableAt HighRepr LogicOp where
  hfoldMap = implHfoldMap $ \case
    LogLit x     -> HRHigh (fromBool x)
    LogNot x     -> HRHigh . sNot . getHrBool $ x
    LogAnd x y   -> appBinop (.&&) x y
    LogOr x y    -> appBinop (.||) x y
    LogImpl x y  -> appBinop (.=>) x y
    LogEquiv x y -> appBinop (.<=>) x y

    where
      appBinop (g :: SBool -> SBool -> SBool) x y =
        HRHigh $ g (getHrBool x) (getHrBool y)
      getHrBool :: HighRepr Bool -> SBool
      getHrBool (HRHigh x) = x
      getHrBool (HRCore x) = case x of -- I.e. absurd

instance (Monad m) => HFoldableAt (Compose m HighRepr) LogicOp where
  hfoldMap = implHfoldMapCompose (pure . hfold)

--------------------------------------------------------------------------------
-- *  Lifting Fortran types to high-level representations

-- | Provides conversions between symbolic representations of core Fortran
-- values and their corresponding high-level types.
class (SymVal a) => LiftD b a | b -> a where
  -- | Go from a core value to the corresponding high-level value.
  liftD :: b -> a
  -- | Go from a symbolic core value to the corresponding symbolic high-level
  -- value.
  liftDRepr :: PrimReprHandlers -> HighRepr b -> HighRepr a

liftDInt :: PrimReprHandlers -> HighRepr (PrimS a) -> HighRepr Integer
liftDInt _ (HRCore (CRPrim _ x)) = HRHigh (SBV (svFromIntegral KUnbounded x))
liftDInt _ _ = error "impossible"


liftDReal :: PrimReprHandlers -> HighRepr (PrimS a) -> HighRepr AlgReal
liftDReal env (HRCore (CRPrim (DPrim prim) x)) =
  HRHigh $ case primSBVKind prim env of
              KFloat -> fromSFloat sRTZ (SBV x)
              KDouble -> fromSDouble sRTZ (SBV x)
              KReal -> SBV x
              k -> error $ "liftDReal: can't convert something of kind " ++
                   show k ++ " to a real"
liftDReal _ _ = error "impossible"

liftDBool :: PrimReprHandlers -> HighRepr (PrimS a) -> HighRepr Bool
liftDBool _ (HRCore (CRPrim _ x)) = HRHigh (SBV (x `svGreaterThan` svFalse))
liftDBool _ _ = error "impossible"


instance LiftD (PrimS Int8) Integer where
  liftD = fromIntegral . getPrimS
  liftDRepr = liftDInt
instance LiftD (PrimS Int16) Integer where
  liftD = fromIntegral . getPrimS
  liftDRepr = liftDInt
instance LiftD (PrimS Int32) Integer where
  liftD = fromIntegral . getPrimS
  liftDRepr = liftDInt
instance LiftD (PrimS Int64) Integer where
  liftD = fromIntegral . getPrimS
  liftDRepr = liftDInt

instance LiftD (PrimS Float) AlgReal where
  liftD = realToFrac . getPrimS
  liftDRepr = liftDReal
instance LiftD (PrimS Double) AlgReal where
  liftD = realToFrac . getPrimS
  liftDRepr = liftDReal

instance LiftD (PrimS Bool8) Bool where
  liftD = (> 0) . getBool8 . getPrimS
  liftDRepr = liftDBool
instance LiftD (PrimS Bool16) Bool where
  liftD = (> 0) . getBool16 . getPrimS
  liftDRepr = liftDBool
instance LiftD (PrimS Bool32) Bool where
  liftD = (> 0) . getBool32 . getPrimS
  liftDRepr = liftDBool
instance LiftD (PrimS Bool64) Bool where
  liftD = (> 0) . getBool64 . getPrimS
  liftDRepr = liftDBool

instance LiftD (PrimS Char8) Word8 where
  liftD = getChar8 . getPrimS
  liftDRepr _ (HRCore (CRPrim _ x)) = HRHigh (sFromIntegral (SBV x :: SBV Word8))
  liftDRepr _ _ =
    error "liftDRepr: a 'PrimS Char8' has a non-primitive representation"

--------------------------------------------------------------------------------
-- * Combinators

-- | Any type that has a core representation has a corresponding Fortran type.
coreReprD :: CoreRepr a -> D a
coreReprD = \case
  CRPrim d _  -> d
  CRArray d _ -> d
  CRData d _  -> d
