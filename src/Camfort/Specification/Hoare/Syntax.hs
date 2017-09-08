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
  deriving (Typeable, Data, Show, Eq, Functor)


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

data AuxDecl ann =
  AuxDecl
  { _adName :: F.Name
  , _adTy :: F.TypeSpec ann
  }
  deriving (Typeable, Data, Show, Eq, Functor)

type PrimSpec ann = Specification (PrimFormula ann)

data SpecOrDecl ann =
    SodSpec (PrimSpec ann)
  | SodDecl (AuxDecl ann)
  deriving (Typeable, Data, Show, Eq, Functor)

instance Show a => Pretty (PrimFormula a) where pretty = show

instance (Pretty a) => Show (Specification a) where
  show Specification { _specType, _specFormula } =
    "Specification { " ++
    "_specType = " ++ show _specType ++ ", " ++
    "_specFormula = " ++ pretty _specFormula ++
    " }"

makeLenses ''Specification
makeLenses ''AuxDecl
makePrisms ''SpecOrDecl

_SpecPre :: Prism' (Specification a) a
_SpecPre = prism' (Specification SpecPre) (\case (Specification SpecPre x) -> Just x; _ -> Nothing)

_SpecPost :: Prism' (Specification a) a
_SpecPost = prism' (Specification SpecPost) (\case (Specification SpecPost x) -> Just x; _ -> Nothing)

_SpecSeq :: Prism' (Specification a) a
_SpecSeq = prism' (Specification SpecSeq) (\case (Specification SpecSeq x) -> Just x; _ -> Nothing)

_SpecInvariant :: Prism' (Specification a) a
_SpecInvariant = prism' (Specification SpecInvariant) (\case (Specification SpecInvariant x) -> Just x; _ -> Nothing)
