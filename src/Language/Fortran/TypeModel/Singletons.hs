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
  data Precision
    = P8
    | P16
    | P32
    | P64
    | P128
    deriving (Show, Eq, Ord)

  data Kind
    = KInt
    | KReal
    | KLogical
    | KChar
{-
    | KProp
    -- ^ A dummy data kind for embedding Fortran expressions in propositions
-}
    deriving (Show, Eq, Ord)

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
{-
    | OKProp
    -- ^ A dummy operator type for embedding Fortran expressions in propositions
-}
    -- deriving (Show, Eq, Ord)

  -- | Finds the maximum of two precision types.
  precMax :: Precision -> Precision -> Precision
  precMax = max

  kindMax :: Kind -> Kind -> Kind
  kindMax = max
   |])
