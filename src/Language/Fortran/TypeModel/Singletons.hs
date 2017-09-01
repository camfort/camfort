{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Fortran.TypeModel.Singletons where

import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.TH

$(singletons
  [d|

  -- | The precision, in bits, of an intrinsic Fortran data type.
  data Precision
    = P8
    | P16
    | P32
    | P64
    | P128
    deriving (Eq, Ord)

  -- | The basic type of an intrinsic Fortran data type.
  data BasicType
    -- NB. The order of the constructors is very important for the derived 'Ord'
    -- instance! Numeric basicTypes that can represent smaller sets of numbers must be
    -- earlier in the list.
    = BTInt
    | BTReal
    | BTLogical
    | BTChar
    deriving (Eq, Ord)

  data OpKind
    = OKLit
    | OKNum
    | OKEq
    | OKRel
    | OKLogical
    | OKLookup
    | OKDeref
    | OKWriteArr
    | OKWriteData

  precMax :: Precision -> Precision -> Precision
  precMax = max

  basicTypeMax :: BasicType -> BasicType -> BasicType
  basicTypeMax = max
   |])

-- These are derived outside the TH splice because we don't need them at the
-- type level so we don't want to waste compilation time on type-level versions
-- of them.

deriving instance Show Precision
deriving instance Show BasicType

deriving instance Eq OpKind
deriving instance Ord OpKind
deriving instance Show OpKind
