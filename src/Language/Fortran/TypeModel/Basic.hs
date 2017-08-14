{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

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
  -- * Helpers
  , SymBool(..)
  , Boolean(..)
  ) where

import           Data.Data
import           Data.Int
import           Data.Word

import           Data.SBV  hiding (KReal, Kind)

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
  toBool :: a -> Bool

numTrue :: Num a => a
numTrue = 1

numFalse :: Num a => a
numFalse = 0

numAnd :: Num a => a -> a -> a
numAnd x y = signum x * signum y

instance SymWord Char8


instance SymWord Bool8

instance SymBool Bool8 where
  toSBool x = x .> 0
  fromSBool b = ite b true false
  toBool x = x > 0

instance Boolean Bool8 where
  true = numTrue
  bnot b = if b == numFalse then numTrue else numFalse
  (&&&) = numAnd

instance Boolean (SBV Bool8) where
  true = numTrue
  bnot b = ite (b .== 0) numTrue numFalse
  (&&&) = numAnd


instance SymWord Bool16

instance SymBool Bool16 where
  toSBool x = x .> 0
  fromSBool b = ite b true false
  toBool x = x > 0

instance Boolean Bool16 where
  true = numTrue
  bnot b = if b == numFalse then numTrue else numFalse
  (&&&) = numAnd

instance Boolean (SBV Bool16) where
  true = numTrue
  bnot b = ite (b .== 0) numTrue numFalse
  (&&&) = numAnd


instance SymWord Bool32

instance SymBool Bool32 where
  toSBool x = x .> 0
  fromSBool b = ite b true false
  toBool x = x > 0

instance Boolean Bool32 where
  true = numTrue
  bnot b = if b == numFalse then numTrue else numFalse
  (&&&) = numAnd

instance Boolean (SBV Bool32) where
  true = numTrue
  bnot b = ite (b .== 0) numTrue numFalse
  (&&&) = numAnd


instance SymWord Bool64

instance SymBool Bool64 where
  toSBool x = x .> 0
  fromSBool b = ite b true false
  toBool x = x > 0

instance Boolean Bool64 where
  true = numTrue
  bnot b = if b == numFalse then numTrue else numFalse
  (&&&) = numAnd

instance Boolean (SBV Bool64) where
  true = numTrue
  bnot b = ite (b .== 0) numTrue numFalse
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
  | KProp
  -- ^ A dummy data kind for embedding Fortran expressions in propositions

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

-- deriving instance Show (Op2Result ok k1 p1 k2 p2 k3 p3)
