{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.Syntax where

import           Data.Data

import           Control.Lens

import qualified Language.Fortran.AST              as F

import           Language.Expression.Pretty

data PrimLogic a
  = PLAnd a a
  | PLOr a a
  | PLImpl a a
  | PLEquiv a a
  | PLNot a
  | PLLit Bool
  deriving (Typeable, Data, Show, Eq, Functor, Foldable, Traversable)


data PrimFormula ann
  = PFExpr (F.Expression ann)
  | PFLogical (PrimLogic (PrimFormula ann))
  deriving (Typeable, Data, Show, Eq)


data SpecKind
  = SpecPre
  | SpecPost
  | SpecSeq
  | SpecInvariant
  deriving (Show, Eq, Typeable, Data)


data Specification a =
  Specification
  { _specType    :: SpecKind
  , _specFormula :: a
  }
  deriving (Typeable, Data, Eq, Functor)

type PrimSpec ann = Specification (PrimFormula ann)

instance Show a => Pretty (PrimFormula a) where pretty = show

instance (Pretty a) => Show (Specification a) where
  show Specification { _specType, _specFormula } =
    "Specification { " ++
    "_specType = " ++ show _specType ++ ", " ++
    "_specFormula = " ++ pretty _specFormula ++
    " }"

makeLenses ''Specification
