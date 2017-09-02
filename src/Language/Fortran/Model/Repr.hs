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

Symbolic representations of Fortran values.

-}
module Language.Fortran.Model.Repr where

import           Data.Int                        (Int16, Int32, Int64, Int8)
import           Data.Word                       (Word8)

import           Data.SBV
import           Data.SBV.Dynamic
import           Data.SBV.Internals              (SBV (..))

import           Data.Vinyl                      hiding (Field)

import           Language.Expression
import           Language.Expression.Prop

import           Language.Fortran.Model.Repr.Prim
import           Language.Fortran.Model.Types

--------------------------------------------------------------------------------
--  Core Fortran Representations
--------------------------------------------------------------------------------

data CoreRepr a where
  SRPrim
    :: D (PrimS a)
    -> SVal
    -> CoreRepr (PrimS a)

  SRArray
    :: D (Array i a)
    -> SArr
    -> CoreRepr (Array i a)

  SRData
    :: D (Record name fs)
    -> Rec (Field CoreRepr) fs
    -> CoreRepr (Record name fs)


coreReprD :: CoreRepr a -> D a
coreReprD = \case
  SRPrim d _  -> d
  SRArray d _ -> d
  SRData d _  -> d

--------------------------------------------------------------------------------
--  High-level data representations
--------------------------------------------------------------------------------

data HighRepr a where
  HRCore :: CoreRepr a -> HighRepr a
  HRHigh :: SBV a -> HighRepr a

instance Applicative f => EvalOp f HighRepr LogicOp where
  evalOp = \case
    LogLit x     -> pure $ HRHigh (fromBool x)
    LogNot x     -> pure $ HRHigh . bnot . getHrBool $ x
    LogAnd x y   -> appBinop (&&&) x y
    LogOr x y    -> appBinop (|||) x y
    LogImpl x y  -> appBinop (==>) x y
    LogEquiv x y -> appBinop (<=>) x y

    where
      appBinop (g :: SBool -> SBool -> SBool) x y =
        pure $ HRHigh $ g (getHrBool x) (getHrBool y)
      getHrBool :: HighRepr Bool -> SBool
      getHrBool (HRHigh x) = x
      getHrBool (HRCore x) = case x of -- I.e. absurd

--------------------------------------------------------------------------------
--  Lifting Fortran types to high-level representations
--------------------------------------------------------------------------------

class (SymWord a) => LiftD b a | b -> a where
  liftD :: b -> a
  liftDRepr :: PrimReprHandlers -> HighRepr b -> HighRepr a

liftDInt :: PrimReprHandlers -> HighRepr (PrimS a) -> HighRepr Integer
liftDInt _ (HRCore (SRPrim _ x)) = HRHigh (SBV (svFromIntegral KUnbounded x))
liftDInt _ _ = error "impossible"


liftDReal :: PrimReprHandlers -> HighRepr (PrimS a) -> HighRepr AlgReal
liftDReal env (HRCore (SRPrim (DPrim prim) x)) =
  HRHigh $ case primSBVKind prim env of
              KFloat -> fromSFloat sRTZ (SBV x)
              KDouble -> fromSDouble sRTZ (SBV x)
              KReal -> SBV x
              k -> error $ "liftDReal: can't convert something of kind " ++
                   show k ++ " to a real"
liftDReal _ _ = error "impossible"

liftDBool :: PrimReprHandlers -> HighRepr (PrimS a) -> HighRepr Bool
liftDBool _ (HRCore (SRPrim _ x)) = HRHigh (SBV (x `svGreaterThan` svFalse))
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
  liftDRepr _ (HRCore (SRPrim _ x)) = HRHigh (sFromIntegral (SBV x :: SBV Word8))
  liftDRepr _ _ =
    error "liftDRepr: a 'PrimS Char8' has a non-primitive representation"
