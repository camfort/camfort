{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wall #-}

{-|

Models the base of the Fortran type system in Haskell types.

-}
module Language.Fortran.TypeModel.Basic
  (
  -- * Type Kinds
    Precision(..)
  , Kind(..)
  -- * Semantic Types
  , Int8
  , Int16
  , Int32
  , Int64
  , Char8(..)
  , Bool8(..)
  , Bool16(..)
  , Bool32(..)
  , Bool64(..)
  -- * Model of Fortran Types
  , D(..)
  ) where
import           Data.Typeable
import           Data.Bits

import           Language.Fortran.TypeModel.Singletons
import           Language.Fortran.TypeModel.TH
import           Language.Fortran.TypeModel.SBV

-- TODO: Complex numbers

--------------------------------------------------------------------------------
--  Semantic type wrappers
--------------------------------------------------------------------------------

symWrapper ''Bool "Bool8" []
symWrapper ''Bool "Bool16" []
symWrapper ''Bool "Bool32" []
symWrapper ''Bool "Bool64" []

symWrapper ''Integer "Int8" [''Num, ''Real, ''Enum, ''Integral, ''Bits, ''SIntegral]
symWrapper ''Integer "Int16" [''Num, ''Real, ''Enum, ''Integral, ''Bits, ''SIntegral]
symWrapper ''Integer "Int32" [''Num, ''Real, ''Enum, ''Integral, ''Bits, ''SIntegral]
symWrapper ''Integer "Int64" [''Num, ''Real, ''Enum, ''Integral, ''Bits, ''SIntegral]

symWrapper ''Float "Float32"
  [''Real, ''Fractional, ''Floating, ''RealFrac,
   ''RealFloat, ''IEEEFloating, ''IEEEFloatConvertable, ''Num]
symWrapper ''Double "Float64"
  [''Real, ''Fractional, ''Floating, ''RealFrac,
   ''RealFloat, ''IEEEFloating, ''IEEEFloatConvertable, ''Num]

symWrapper ''Integer "Char8" []


instance SymBool Bool8 where
  fromSBool = wrapSym
  toSBool = unwrapSym
instance SymBool Bool16 where
  fromSBool = wrapSym
  toSBool = unwrapSym
instance SymBool Bool32 where
  fromSBool = wrapSym
  toSBool = unwrapSym
instance SymBool Bool64 where
  fromSBool = wrapSym
  toSBool = unwrapSym

--------------------------------------------------------------------------------
--  Model of Fortran Types
--------------------------------------------------------------------------------

-- | Lists the allowed Fortran types, with corresponding constraints on
-- precision, kind and semantic Haskell type.
data D p k a where
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

  DProp          :: D 'P64  'KProp    Bool

deriving instance Show (D p k a)
deriving instance Typeable (D p k a)
