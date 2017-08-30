{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.Translate.Types where

import           Data.List                       (intersperse)
import           Data.Typeable                   (Typeable)

import           Data.Map                        (Map)

import           Control.Lens                    hiding (Const (..), op, rmap)
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Vinyl
import           Data.Vinyl.Functor              (Const (..))

import           Data.SBV.Dynamic                (svFalse, svGreaterThan)
import           Data.SBV.Internals              (SBV (SBV))

import qualified Language.Fortran.AST            as F

import           Language.Expression.DSL
import           Language.Expression.Pretty

import           Camfort.Analysis.Logger
import           Language.Fortran.TypeModel
import           Language.Fortran.TypeModel.Vars


--------------------------------------------------------------------------------
--  Lifting Logical Values
--------------------------------------------------------------------------------

-- | Propositions expect values of type 'Bool', so this is necessary to do the
-- conversion.
data FLiftLogical t a where
  FLL8  :: t (PrimS Bool8) -> FLiftLogical t Bool
  FLL16 :: t (PrimS Bool16) -> FLiftLogical t Bool
  FLL32 :: t (PrimS Bool32) -> FLiftLogical t Bool
  FLL64 :: t (PrimS Bool64) -> FLiftLogical t Bool

instance Operator FLiftLogical where
  htraverseOp f = \case
    FLL8 x -> FLL8 <$> f x
    FLL16 x -> FLL16 <$> f x
    FLL32 x -> FLL32 <$> f x
    FLL64 x -> FLL64 <$> f x

instance (Applicative f) => EvalOp f SymRepr FLiftLogical where
  evalOp f = \case
    FLL8  x -> primToBool <$> f x
    FLL16 x -> primToBool <$> f x
    FLL32 x -> primToBool <$> f x
    FLL64 x -> primToBool <$> f x
    where
      primToBool :: SymRepr (PrimS a) -> SymRepr Bool
      primToBool (SRPrim _ v) = SRProp (SBV (v `svGreaterThan` svFalse))

instance Pretty2 FLiftLogical where
  prettys2Prec p = \case
    FLL8  x -> prettys1Prec p x
    FLL16 x -> prettys1Prec p x
    FLL32 x -> prettys1Prec p x
    FLL64 x -> prettys1Prec p x

--------------------------------------------------------------------------------
--  General types
--------------------------------------------------------------------------------

type FortranExpr = Expr FortranOp FortranVar

type FExpr = Expr' '[FortranOp, FLiftLogical]
type TransFormula = PropOver (FExpr FortranVar)

data Some f where
  Some :: f a -> Some f

data PairOf f g a where
  PairOf :: f a -> g a -> PairOf f g a

pattern SomePair :: f a -> g a -> Some (PairOf f g)
pattern SomePair x y = Some (PairOf x y)

instance Pretty1 f => Pretty (Some f) where
  prettysPrec p = \case
    Some x -> prettys1Prec p x

instance Pretty1 g => Pretty1 (PairOf f g) where
  prettys1Prec p = \case
    PairOf _ x -> prettys1Prec p x

instance Pretty1 f => Show (Some f) where
  show = pretty

traverseSome
  :: Applicative m
  => (forall a. f a -> m (g a))
  -> Some f -> m (Some g)
traverseSome f (Some x) = Some <$> f x

traversePairOf
  :: Applicative m
  => (f a -> g a -> m (f' b, g' b))
  -> PairOf f g a -> m (PairOf f' g' b)
traversePairOf f (PairOf x y) = uncurry PairOf <$> f x y

mapSome :: (forall a. f a -> g a) -> Some f -> Some g
mapSome f = runIdentity . traverseSome (Identity . f)

class Trivial a
instance Trivial a

type SomeVar = Some FortranVar
type SomeExpr = Some (PairOf D FortranExpr)
type SomeType = Some D

someVarName :: SomeVar -> String
someVarName (Some (FortranVar _ np)) = getUniqueName $ _npUnique np

--------------------------------------------------------------------------------
--  Translate Monad
--------------------------------------------------------------------------------

data TranslateEnv =
  TranslateEnv
  { _teImplictVars :: Bool
  , _teVarsInScope :: Map SourceName SomeVar
  }

newtype TranslateT m a =
  TranslateT
  { getTranslateT
    :: ReaderT TranslateEnv (ExceptT TranslateError m) a
  }
  deriving ( Functor, Applicative, Monad
           , MonadError TranslateError
           , MonadReader TranslateEnv
           , MonadLogger e w
           )

runTranslateT
  :: (Monad m)
  => TranslateT m a
  -> TranslateEnv
  -> m (Either TranslateError a)
runTranslateT (TranslateT action) env = runExceptT $ runReaderT action env

--------------------------------------------------------------------------------
--  Errors
--------------------------------------------------------------------------------

data TranslateError
  = ErrUnsupportedItem Text
  -- ^ Tried to translate a part of the language that is not (yet) supported.
  | ErrBadLiteral
  -- ^ Found a literal value that we didn't know how to translate. May or may
  -- not be valid Fortran.
  | ErrUnexpectedType Text SomeType SomeType
  -- ^ Tried to translate a FORTRAN language part into the wrong expression
  -- type, and it wasn't coercible to the correct type.
  | ErrInvalidOpApplication (Some (Rec D))
  -- ^ Tried to apply an operator to arguments with the wrong types.
  | ErrVarNotInScope F.Name
  -- ^ Reference to a variable that's not currently in scope
  deriving (Typeable)

instance Describe TranslateError where
  describeBuilder = \case
    ErrUnsupportedItem message ->
      "unsupported " <> describeBuilder message

    ErrBadLiteral ->
      "encountered a literal value that couldn't be translated; " <>
      "it might be invalid Fortran or it might use unsupported language features"

    ErrUnexpectedType message ty1 ty2 ->
      "unexpected type in " <> describeBuilder message <>
      "; expected type was '" <> describeBuilder (show ty1) <>
      "'; actual type was '" <> describeBuilder (show ty2) <> "'"

    ErrInvalidOpApplication (Some argTypes) ->
      let descTypes
            = recordToList
            . rmap (Const . surround "'" . describeBuilder . pretty1)
            $ argTypes
          surround s x = s <> x <> s
      in "tried to apply operator to arguments of the wrong type; arguments had types " <>
         mconcat (intersperse ", " descTypes)

    ErrVarNotInScope nm ->
      "Reference to variable '" <> describeBuilder nm <> "' which is not in scope"

--------------------------------------------------------------------------------
--  Lenses
--------------------------------------------------------------------------------

makeLenses ''TranslateEnv

--------------------------------------------------------------------------------
--  Translation Environments
--------------------------------------------------------------------------------

defaultTranslateEnv :: TranslateEnv
defaultTranslateEnv =
  TranslateEnv
  { _teImplictVars = False
  , _teVarsInScope = mempty
  }
