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
{-# LANGUAGE TypeApplications          #-}

{-|

Data kinds and corresponding singletons (via the @singletons@ library) for kinds
used in various places in "Language.Fortran.Model".

As documentation in Template Haskell is not yet supported, documentation for
each data type is given here.

== 'Precision'

The precision, in bits, of an intrinsic Fortran data type.

== 'BasicType'

The basic type of an intrinsic Fortran data type.

== 'OpKind'

TODO

== 'precMax'

Finds the maximum of two precisions. Use 'PrecMax' at the type level and 'sPrecMax' for singletons.

== 'basicTypeMax'

Finds the \'largest\' (with respect to the size of the set it semantically
represents) of numeric basic types. Also works with non-numeric basic types, but
the result in that case is unspecified. Use 'BasicTypeMax' at the type level and
'sBasicTypeMax' for singletons.

-}
module Language.Fortran.Model.Singletons where

import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons
  [d|

  data Precision
    = P8
    | P16
    | P32
    | P64
    | P128
    deriving (Eq, Ord)

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
