{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -Wall      #-}

module Language.Fortran.Model.Op.High.Repr where

import           Data.Int                        (Int16, Int32, Int64, Int8)
import           Data.Word                       (Word8)

import           Control.Monad.Reader.Class      (MonadReader (ask))

import           Data.SBV
import           Data.SBV.Dynamic
import           Data.SBV.Internals              (SBV (..))

import           Language.Expression
import           Language.Expression.Pretty

import           Language.Fortran.Model.EvalPrim
import           Language.Fortran.Model.Types

--------------------------------------------------------------------------------
--  Meta-level data representations
--------------------------------------------------------------------------------

data MetaRepr a where
  SymRepr :: SymRepr a -> MetaRepr a
  SbvRepr :: SBV a -> MetaRepr a

--------------------------------------------------------------------------------
--  Lifting Fortran types to high-level representations
--------------------------------------------------------------------------------

class (SymWord a) => LiftD b a | b -> a where
  liftD :: b -> a
  liftDRepr :: SymReprEnv -> MetaRepr b -> MetaRepr a

liftDInt :: SymReprEnv -> MetaRepr (PrimS a) -> MetaRepr Integer
liftDInt _ (SymRepr (SRPrim _ x)) = SbvRepr (SBV (svFromIntegral KUnbounded x))
liftDInt _ _ = error "impossible"


liftDReal :: SymReprEnv -> MetaRepr (PrimS a) -> MetaRepr AlgReal
liftDReal env (SymRepr (SRPrim (DPrim prim) x)) =
  SbvRepr $ case primSBVKind prim env of
              KFloat -> fromSFloat sRTZ (SBV x)
              KDouble -> fromSDouble sRTZ (SBV x)
              KReal -> SBV x
              k -> error $ "liftDReal: can't convert something of kind " ++ show k ++ " to a real"
liftDReal _ _ = error "impossible"

liftDBool :: SymReprEnv -> MetaRepr (PrimS a) -> MetaRepr Bool
liftDBool _ (SymRepr (SRPrim _ x)) = SbvRepr (SBV (x `svGreaterThan` svFalse))
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
  liftDRepr _ (SymRepr (SRPrim _ x)) = SbvRepr (sFromIntegral (SBV x :: SBV Word8))
  liftDRepr _ _ =
    error "liftDRepr: a 'PrimS Char8' has a non-primitive representation"

--------------------------------------------------------------------------------
--  Arrays
--------------------------------------------------------------------------------

-- data ArrayOp t a where
--   OpSum :: NumericBasicType k -> Prim p k a -> t (Array i (PrimS a)) -> ArrayOp t (PrimS a)
