{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Fortran.TypeModel.Singletons where

import           Data.Singletons.TH
import           Data.Singletons.Prelude

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
    = KReal
    | KInt
    | KLogical
    | KChar
    | KProp
    -- ^ A dummy data kind for embedding Fortran expressions in propositions
    deriving (Show, Eq, Ord)

  data OpKind
    = OpNum
    | OpEquality
    | OpRelational
    | OpLogical
    | OpProp
    -- ^ A dummy operator type for embedding Fortran expressions in propositions
    deriving (Show, Eq, Ord)
   |])
