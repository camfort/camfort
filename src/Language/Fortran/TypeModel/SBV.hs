{-# LANGUAGE TypeFamilies #-}

module Language.Fortran.TypeModel.SBV
  (
  -- * Classes
    SymWord(..)
  , WrappedSym(..)
  -- * SBV re-exports
  , SBV
  , SymBool(..)
  , SIntegral
  , sFromIntegral
  , Boolean(..)
  , IEEEFloatConvertable(..)
  , IEEEFloating(..)
  , SDivisible(..)
  , OrdSymbolic(..)
  , sRTZ
  -- * Unsafe
  , unsafeTransmuteSBV
  ) where

import           Data.SBV
import qualified Data.SBV.Internals as S

class WrappedSym a where
  type UnwrappedSym a
  wrapSym :: SBV (UnwrappedSym a) -> SBV a
  unwrapSym :: SBV a -> SBV (UnwrappedSym a)


class SymBool b where
  toSBool :: SBV b -> SBool
  fromSBool :: SBool -> SBV b


unsafeTransmuteSBV :: SBV a -> SBV b
unsafeTransmuteSBV = S.SBV . S.unSBV
