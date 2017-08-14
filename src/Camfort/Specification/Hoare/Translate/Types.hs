{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.Translate.Types where

import           Control.Exception          (Exception (..))
import           Data.Typeable              (TypeRep, Typeable, Proxy(..), gcast, typeRep)

import           Data.Map                   (Map)

import           Control.Lens               hiding (op)
import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Language.Fortran.AST       as F

import           Language.Expression.DSL
import           Language.Expression.Pretty
import           Language.Verification

import           Language.Fortran.TypeModel
import           Language.Fortran.TypeModel.DSL hiding (FExpr)

--------------------------------------------------------------------------------
--  General types
--------------------------------------------------------------------------------

data Some c f where
  Some :: (c a, Typeable a) => D p k a -> f a -> Some c f

_Some
  :: (p a, Typeable a, p b, Typeable b, HasRepr a, HasRepr b)
  => Prism (Some p f) (Some p f) (f a) (f b)
_Some = prism (Some (dForType Proxy)) extract
  where
    extract :: (p a, Typeable a) => Some p f -> Either (Some p f) (f a)
    extract (Some d e) = maybe (Left (Some d e)) Right (gcast e)

traverseSome
  :: Applicative m
  => (forall a p k. (c a, Typeable a) => D p k a -> f a -> m (g a))
  -> Some c f -> m (Some c g)
traverseSome f (Some d x) = Some d <$> f d x

mapSome :: (forall a p k. (c a, Typeable a) => D p k a -> f a -> g a) -> Some c f -> Some c g
mapSome f = runIdentity . traverseSome (\d -> Identity . f d)

-- | @'Some' f p@ contains an @f a@ for some @a@. This function gets the
-- 'TypeRep' of @a@.
someTypeRep :: Some p f -> TypeRep
someTypeRep (Some _ x) = typeRep x

type SomeVar l = Some Verifiable (Var l)
type SomeExpr = Some SymWord FortranExpr
type SomeType = Some SymWord (Const ())

someType :: (Typeable a, SymWord a) => D p k a -> SomeType
someType d = Some d (Const ())

someVarName :: Location l => SomeVar l -> String
someVarName (Some _ v) = varName v

newtype SourceName = SourceName { getSourceName :: F.Name }
  deriving (Eq, Ord)

instance Show SourceName where show = show . getSourceName

newtype UniqueName = UniqueName { getUniqueName :: F.Name }
  deriving (Eq, Ord)

instance Show UniqueName where show = show . getUniqueName

-- | A 'NamePair' represents the name of some part of a Fortran program,
-- including the human-readable source name and the unique name.
data NamePair =
  NamePair
  { _npUnique :: UniqueName
  , _npSource :: SourceName
  }
  deriving (Eq, Ord, Show)

-- | The location name of a 'NamePair' is its unique name.
instance Location NamePair where
  locationName (NamePair { _npUnique }) = getUniqueName _npUnique

-- | The pretty version of a 'NamePair' is its source name.
instance Pretty NamePair where
  pretty (NamePair { _npSource }) = getSourceName _npSource

--------------------------------------------------------------------------------
--  Translate Monad
--------------------------------------------------------------------------------

data TranslateEnv =
  TranslateEnv
  { _teImplictVars :: Bool
  , _teVarsInScope :: Map SourceName (SomeVar NamePair)
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

type FortranExpr = Expr FortranOp (Var NamePair)

type TransFormula = PropOver FortranExpr

--------------------------------------------------------------------------------
--  Errors
--------------------------------------------------------------------------------

data TranslateError a
  = ErrUnsupportedItem (LangPart a)
  -- ^ Tried to translate a part of the language that is not (yet) supported.
  | ErrBadLiteral (F.Value a)
  -- ^ Found a literal value that we didn't know how to translate. May or may
  -- not be valid Fortran.
  | ErrUnexpectedType (LangPart a) TypeRep
  -- ^ Tried to translate a FORTRAN language part into the wrong expression
  -- type, and it wasn't coercible to the correct type.
  | ErrInvalidVarType TypeRep
  -- ^ Tried to make a variable representing a value of a type that can't be
  -- stored in a variable.
  | ErrInvalidOperatorApplication (F.Expression a, TypeRep) (F.Expression a, TypeRep) (LangPart a)
  -- ^ Tried to apply an operator to arguments with the wrong type.
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
      "Found a literal value that couldn't be translated;" ++
      "it might be invalid Fortran or it might use unsupported language features: " ++
      displayValue v

    ErrUnexpectedType lp ty ->
      "Language item had unexpected type: language item was " ++ displayLangPart lp ++
      "; expected type was " ++ show ty

    ErrInvalidVarType ty ->
      "Tried to make a variable containing unsupported variable type " ++ show ty

    ErrInvalidOperatorApplication (e1, ty1) (e2, ty2) lp ->
      "Tried to apply operator to arguments of the wrong type: Operator was " ++ displayLangPart lp ++
      "; left operand was " ++ displayExpression e1 ++ " of type " ++ show ty1 ++
      "; right operand was " ++ displayExpression e2 ++ " of type " ++ show ty2

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

errUnexpectedType :: MonadError (TranslateError ann) m => LangPart ann -> TypeRep -> m x
errUnexpectedType lp = throwError . ErrUnexpectedType lp

errInvalidVarType :: MonadError (TranslateError ann) m => TypeRep -> m x
errInvalidVarType = throwError . ErrInvalidVarType

errInvalidOperatorApplication :: MonadError (TranslateError ann) m => (F.Expression ann, TypeRep) -> (F.Expression ann, TypeRep) -> LangPart ann -> m x
errInvalidOperatorApplication e1 e2 lp = throwError (ErrInvalidOperatorApplication e1 e2 lp)

errVarNotInScope :: MonadError (TranslateError ann) m => F.Name -> m x
errVarNotInScope = throwError . ErrVarNotInScope

--------------------------------------------------------------------------------
--  Lenses
--------------------------------------------------------------------------------

makeLenses ''TranslateEnv

makeWrapped ''SourceName
makeWrapped ''UniqueName
makeLenses ''NamePair

--------------------------------------------------------------------------------
--  Translation Environments
--------------------------------------------------------------------------------

defaultTranslateEnv :: TranslateEnv
defaultTranslateEnv =
  TranslateEnv
  { _teImplictVars = False
  , _teVarsInScope = mempty
  }
