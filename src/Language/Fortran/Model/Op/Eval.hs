{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall #-}

module Language.Fortran.Model.Op.Eval where

import           Control.Monad.Reader.Class       (MonadReader (..))

import           Data.SBV                         (SDouble, SFloat, SReal, sRTZ)
import qualified Data.SBV                         as SBV
import           Data.SBV.Dynamic                 (SVal)
import qualified Data.SBV.Dynamic                 as SBV
import           Data.SBV.Internals               (SBV (..))

import           Language.Fortran.Model.Repr.Prim
import           Language.Fortran.Model.Types

--------------------------------------------------------------------------------
--  Monad
--------------------------------------------------------------------------------

class (MonadReader r m, HasPrimReprHandlers r) => MonadEvalFortran r m | m -> r where
instance (MonadReader r m, HasPrimReprHandlers r) => MonadEvalFortran r m where

--------------------------------------------------------------------------------
--  SBV Kinds
--------------------------------------------------------------------------------

coerceBy :: (SBV a -> SBV b) -> SVal -> SVal
coerceBy f x = unSBV (f (SBV x))

coerceSBVKinds :: SBV.Kind -> SBV.Kind -> (SVal -> SVal)
coerceSBVKinds SBV.KReal   SBV.KReal = id
coerceSBVKinds SBV.KFloat  SBV.KReal = coerceBy (SBV.fromSFloat sRTZ :: SFloat -> SReal)
coerceSBVKinds SBV.KDouble SBV.KReal = coerceBy (SBV.fromSDouble sRTZ :: SDouble -> SReal)
coerceSBVKinds _        k2@SBV.KReal = SBV.svFromIntegral k2

coerceSBVKinds SBV.KReal   SBV.KDouble = coerceBy (SBV.toSDouble sRTZ :: SReal -> SDouble)
coerceSBVKinds SBV.KDouble SBV.KDouble = id
coerceSBVKinds SBV.KFloat  SBV.KDouble = coerceBy (SBV.toSDouble sRTZ :: SFloat -> SDouble)
coerceSBVKinds _        k2@SBV.KDouble = SBV.svFromIntegral k2

coerceSBVKinds SBV.KReal   SBV.KFloat = coerceBy (SBV.toSFloat sRTZ :: SReal -> SFloat)
coerceSBVKinds SBV.KDouble SBV.KFloat = coerceBy (SBV.toSFloat sRTZ :: SDouble -> SFloat)
coerceSBVKinds SBV.KFloat  SBV.KFloat = id
coerceSBVKinds _        k2@SBV.KFloat = SBV.svFromIntegral k2

coerceSBVKinds _ k2 = SBV.svFromIntegral k2

coercePrimSVal :: (MonadEvalFortran r m) => Prim p k a -> SVal -> m SVal
coercePrimSVal p v = do
  k2 <- primSBVKind p
  let k1 = SBV.kindOf v
  return $ coerceSBVKinds k1 k2 v
