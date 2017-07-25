{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PatternSynonyms         #-}

module Camfort.Specification.Hoare.Syntax where

import           Control.Monad                     (ap, join)
import           Data.Data
import           GHC.Generics

import           Data.Generics.Uniplate.Operations

import qualified Language.Fortran.AST              as F
import qualified Language.Fortran.Util.Position    as F

import           Language.While.Prop


data SpecKind
  = SpecPre
  | SpecPost
  | SpecSeq
  | SpecInvariant


data SpecOp a
  = OpEq a a
  | OpLT a a
  | OpLE a a
  | OpGT a a
  | OpGE a a
  deriving (Show, Data, Typeable, Functor, Foldable, Traversable)


type SpecFormula a = Prop (SpecOp (F.Expression a))

pattern FEq a b = PEmbed (OpEq a b)
pattern FLT a b = PEmbed (OpLT a b)
pattern FLE a b = PEmbed (OpLE a b)
pattern FGT a b = PEmbed (OpGT a b)
pattern FGE a b = PEmbed (OpGE a b)


bindExpVars
  :: (Data a, Monad m)
  => (a -> F.SrcSpan -> F.Name -> m (F.Expression a))
  -> F.Expression a
  -> m (F.Expression a)
bindExpVars f = transformM $ \case
  F.ExpValue ann span (F.ValVariable nm) -> f ann span nm
  exp -> pure exp


bindFormula
  :: (Data a, Monad m)
  => (a -> F.SrcSpan -> F.Name -> m (F.Expression a))
  -> SpecFormula a -> m (SpecFormula a)
bindFormula = traverse . traverse . bindExpVars


data Specification a =
  Specification
  { _specType    :: SpecKind
  , _specFormula :: SpecFormula a
  }

