{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.Translate.Types where

import           Control.Exception          (Exception (..))
import           Data.Typeable              (Typeable)

import           Data.Map                   (Map)

import           Control.Lens               hiding (op)
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.SBV.Dynamic           (svFalse, svGreaterThan)
import           Data.SBV.Internals         (SBV (SBV))

import qualified Language.Fortran.AST       as F

import           Language.Expression.DSL
import           Language.Expression.Pretty

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
  Some :: D a -> f a -> Some f

instance Pretty1 f => Pretty (Some f) where
  prettysPrec p = \case
    Some _ x -> prettys1Prec p x

instance Pretty1 f => Show (Some f) where
  show = pretty

traverseSome
  :: Applicative m
  => (forall a. f a -> m (g a))
  -> Some f -> m (Some g)
traverseSome f (Some d x) = Some d <$> f x

mapSome :: (forall a. f a -> g a) -> Some f -> Some g
mapSome f = runIdentity . traverseSome (Identity . f)

class Trivial a
instance Trivial a

type SomeVar = Some FortranVar
type SomeExpr = Some FortranExpr
type SomeType = Some D

someType :: D a -> SomeType
someType d = Some d d

someVarName :: SomeVar -> String
someVarName (Some _ (FortranVar _ np)) = getUniqueName $ _npUnique np

--------------------------------------------------------------------------------
--  Translate Monad
--------------------------------------------------------------------------------

data TranslateEnv =
  TranslateEnv
  { _teImplictVars :: Bool
  , _teVarsInScope :: Map SourceName SomeVar
  }

newtype MonadTranslate ann a =
  MonadTranslate
  { getMonadTranslate
    :: ReaderT TranslateEnv (Either (TranslateError ann)) a
  }
  deriving ( Functor, Applicative, Monad
           , MonadError (TranslateError ann)
           , MonadReader TranslateEnv)

runMonadTranslate
  :: MonadTranslate ann a -> TranslateEnv -> Either (TranslateError ann) a
runMonadTranslate (MonadTranslate action) env = runReaderT action env

--------------------------------------------------------------------------------
--  Errors
--------------------------------------------------------------------------------

data TranslateError a
  = ErrUnsupportedItem (LangPart a)
  -- ^ Tried to translate a part of the language that is not (yet) supported.
  | ErrBadLiteral (F.Value a)
  -- ^ Found a literal value that we didn't know how to translate. May or may
  -- not be valid Fortran.
  | ErrUnexpectedType (LangPart a) SomeType SomeType
  -- ^ Tried to translate a FORTRAN language part into the wrong expression
  -- type, and it wasn't coercible to the correct type.
  | ErrInvalidVarType SomeType
  -- ^ Tried to make a variable representing a value of a type that can't be
  -- stored in a variable.
  | ErrInvalidBinopApplication (F.Expression a, SomeType) (F.Expression a, SomeType) (LangPart a)
  -- ^ Tried to apply a binary operator to arguments with the wrong types.
  | ErrInvalidUnopApplication (F.Expression a, SomeType) (LangPart a)
  -- ^ Tried to apply a unary operator to an argument with the wrong type.
  | ErrVarNotInScope F.Name
  -- ^ Reference to a variable that's not currently in scope
  deriving (Typeable, Show)

data LangPart a
  = LpExpression (F.Expression a)
  | LpValue (F.Value a)
  | LpTypeSpec (F.TypeSpec a)
  | LpUnaryOp F.UnaryOp
  | LpBinaryOp F.BinaryOp
  deriving (Typeable, Show)

displayLangPart :: (Show ann) => LangPart ann -> String
displayLangPart = \case
  LpExpression e -> "Expression '" ++ displayExpression e ++ "'"
  LpValue v -> "Value '" ++ displayValue v ++ "'"
  LpTypeSpec ts -> "TypeSpec '" ++ displayTypeSpec ts ++ "'"
  LpUnaryOp op -> "Unary '" ++ displayUnaryOp op ++ "'"
  LpBinaryOp op -> "Binary '" ++ displayBinaryOp op ++ "'"

displayValue :: (Show ann) => F.Value ann -> String
displayValue _ = "<unimplemented displayValue>"

displayExpression :: (Show ann) => F.Expression ann -> String
displayExpression _ = "<unimplemented displayExpression>"
-- displayExpression = show

displayTypeSpec :: (Show ann) => F.TypeSpec ann -> String
displayTypeSpec _ = "<unimplemented displayTypeSpec>"

displayUnaryOp :: F.UnaryOp -> String
displayUnaryOp = \case
  F.Plus -> "+"
  F.Minus -> "-"
  F.Not -> "! <not>"
  F.UnCustom s -> s ++ " <custom operator>"

displayBinaryOp :: F.BinaryOp -> String
displayBinaryOp = \case
  F.Addition -> "+"
  F.Subtraction -> "-"
  F.Multiplication -> "*"
  F.Division -> "/"
  F.Exponentiation -> "^ <exponentiation>"
  F.Concatenation -> "& <concatenation>"
  F.GT -> ">"
  F.GTE -> ">="
  F.LT -> "<"
  F.LTE -> "<="
  F.EQ -> "=="
  F.NE -> "/="
  F.Or -> "||"
  F.And -> "&&"
  F.Equivalent -> "=== <equivalent>"
  F.NotEquivalent -> "!== <not equivalent>"
  F.BinCustom s -> s ++ " <custom operator>"

instance (Typeable ann, Show ann) => Exception (TranslateError ann) where
  displayException = \case
    ErrUnsupportedItem lp ->
      "Unsupported language item " ++ displayLangPart lp

    ErrBadLiteral v ->
      "Found a literal value that couldn't be translated; " ++
      "it might be invalid Fortran or it might use unsupported language features: " ++
      displayValue v

    ErrUnexpectedType lp ty1 ty2 ->
      "Language item had unexpected type: language item was " ++ displayLangPart lp ++
      "; expected type was " ++ show ty1 ++ "; actual type was " ++ show ty2

    ErrInvalidVarType ty ->
      "Tried to make a variable containing unsupported variable type " ++ show ty

    ErrInvalidBinopApplication (e1, ty1) (e2, ty2) lp ->
      "Tried to apply operator to arguments of the wrong type: Operator was " ++ displayLangPart lp ++
      "; left operand was " ++ displayExpression e1 ++ " of type " ++ show ty1 ++
      "; right operand was " ++ displayExpression e2 ++ " of type " ++ show ty2

    ErrInvalidUnopApplication (e, ty) lp ->
      "Tried to apply unary operator to arguments of the wrong type: Operator was " ++ displayLangPart lp ++
      "; operand was " ++ displayExpression e ++ " of type " ++ show ty

    ErrVarNotInScope nm ->
      "Reference to variable '" ++ nm ++ "' which is not in scope"

errUnsupportedItem :: MonadError (TranslateError ann) m => LangPart ann -> m x
errUnsupportedItem = throwError . ErrUnsupportedItem

errUnsupportedExpression :: MonadError (TranslateError ann) m => F.Expression ann -> m x
errUnsupportedExpression = errUnsupportedItem . LpExpression

errUnsupportedValue :: MonadError (TranslateError ann) m => F.Value ann -> m x
errUnsupportedValue = errUnsupportedItem . LpValue

errUnsupportedTypeSpec :: MonadError (TranslateError ann) m => F.TypeSpec ann -> m x
errUnsupportedTypeSpec = errUnsupportedItem . LpTypeSpec

errBadLiteral :: MonadError (TranslateError ann) m => (F.Value ann) -> m x
errBadLiteral = throwError . ErrBadLiteral

errUnexpectedType :: MonadError (TranslateError ann) m => LangPart ann -> SomeType -> SomeType -> m x
errUnexpectedType lp tye = throwError . ErrUnexpectedType lp tye

errInvalidVarType :: MonadError (TranslateError ann) m => SomeType -> m x
errInvalidVarType = throwError . ErrInvalidVarType

errInvalidBinopApplication
  :: MonadError (TranslateError ann) m
  => (F.Expression ann, SomeType) -> (F.Expression ann, SomeType) -> LangPart ann -> m x
errInvalidBinopApplication e1 e2 lp = throwError (ErrInvalidBinopApplication e1 e2 lp)

errInvalidUnopApplication
  :: MonadError (TranslateError ann) m
  => (F.Expression ann, SomeType) -> LangPart ann -> m x
errInvalidUnopApplication e lp = throwError (ErrInvalidUnopApplication e lp)

errVarNotInScope :: MonadError (TranslateError ann) m => F.Name -> m x
errVarNotInScope = throwError . ErrVarNotInScope

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
