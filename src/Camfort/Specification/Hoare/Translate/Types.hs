{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Camfort.Specification.Hoare.Translate.Types where

import           Control.Exception           (Exception (..))
import           Data.Typeable               (Proxy (..), TypeRep, Typeable,
                                              gcast, typeRep)

import           Data.Map                    (Map)

import           Control.Lens                hiding (op)
import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Language.Fortran.AST        as F

import           Language.Expression.Classes
import           Language.Expression.Dict
import           Language.Expression.Constraints
import           Language.Verification

--------------------------------------------------------------------------------
--  Existential Types
--------------------------------------------------------------------------------

data Some p f where
  Some :: (p a, Typeable a) => f a -> Some p f

_Some :: (p a, Typeable a, p b, Typeable b) => Prism (Some p f) (Some p f) (f a) (f b)
_Some = prism Some extract
  where
    extract :: (p a, Typeable a) => Some p f -> Either (Some p f) (f a)
    extract (Some e) = maybe (Left (Some e)) Right (gcast e)

-- | @'Some' f p@ contains an @f a@ for some @a@. This function gets the
-- 'TypeRep' of @a@.
someTypeRep :: Some p f -> TypeRep
someTypeRep (Some (_ :: f a)) = typeRep (Proxy :: Proxy a)

type SomeExpr = Some SymValue FortranExpr
type SomeVar l = Some Verifiable (Var l)

--------------------------------------------------------------------------------
--  Translate Monad
--------------------------------------------------------------------------------

data TranslateEnv =
  TranslateEnv
  { _teImplicitVarTypes :: Bool
  , _teExistingVars :: Map F.Name (SomeVar String)

  , _teNumDicts :: Typemap NumDict
  , _teEqDicts :: Typemap2 EqDict
  , _teOrdDicts :: Typemap2 OrdDict
  , _teBooleanDicts :: Typemap BooleanDict

  , _teSymNumInstances :: Dictmap SymNum
  , _teSymBoolInstances :: Dictmap SymBool
  , _teSymEqInstances :: Dictmap2 SymEq
  , _teSymOrdInstances :: Dictmap2 SymOrd
  }

newtype MonadTranslate ann a =
  MonadTranslate { getMonadTranslate :: ReaderT TranslateEnv (Either (TranslateError ann)) a }
  deriving (Functor, Applicative, Monad, MonadError (TranslateError ann), MonadReader TranslateEnv)

type FortranOps = CoerceOp : StandardOps

type FortranExpr = Expr' FortranOps (Var String)

runMonadTranslate :: MonadTranslate ann a -> TranslateEnv -> Either (TranslateError ann) a
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
  | ErrUnexpectedType (LangPart a) TypeRep
  -- ^ Tried to translate a FORTRAN language part into the wrong expression
  -- type, and it wasn't coercible to the correct type.
  | ErrInvalidVarType TypeRep
  -- ^ Tried to make a variable representing a value of a type that can't be
  -- stored in a variable.
  | ErrInvalidOperatorApplication (LangPart a)
  -- ^ Tried to apply an operator to arguments with the wrong type.
  deriving (Typeable, Show)

data LangPart a
  = LpExpression (F.Expression a)
  | LpValue (F.Value a)
  | LpUnaryOp F.UnaryOp
  | LpBinaryOp F.BinaryOp
  deriving (Typeable, Show)

displayLangPart :: LangPart ann -> String
displayLangPart = \case
  LpExpression e -> "Expression '" ++ displayExpression e ++ "'"
  LpValue v -> "Value '" ++ displayValue v ++ "'"
  LpUnaryOp op -> "Unary '" ++ displayUnaryOp op ++ "'"
  LpBinaryOp op -> "Binary '" ++ displayBinaryOp op ++ "'"

displayValue :: F.Value ann -> String
displayValue _ = "<unimplemented displayValue>"

displayExpression :: F.Expression ann -> String
displayExpression _ = "<unimplemented displayExpression>"

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
  F.NE -> "!="
  F.Or -> "||"
  F.And -> "&&"
  F.Equivalent -> "=== <equivalent>"
  F.NotEquivalent -> "!== <not equivalent>"
  F.BinCustom s -> s ++ " <custom operator>"

instance (Typeable ann, Show ann) => Exception (TranslateError ann) where
  displayException = \case
    ErrUnsupportedItem lp ->
      "Unsupported language item: " ++ displayLangPart lp

    ErrBadLiteral v ->
      "Found a literal value that couldn't be translated." ++
      "It might be invalid Fortran or it might use unsupported language features: " ++
      displayValue v

    ErrUnexpectedType lp ty ->
      "Language item had unexpected type. Language item was: " ++ displayLangPart lp ++
      ". Expected type was: " ++ show ty ++ "."

    ErrInvalidVarType ty ->
      "Tried to make a variable containing unsupported variable type: " ++ show ty

    ErrInvalidOperatorApplication lp ->
      "Tried to apply operator to arguments of the wrong type: " ++ displayLangPart lp

errUnsupportedItem :: MonadError (TranslateError ann) m => LangPart ann -> m x
errUnsupportedItem = throwError . ErrUnsupportedItem

errUnsupportedExpression :: MonadError (TranslateError ann) m => F.Expression ann -> m x
errUnsupportedExpression = errUnsupportedItem . LpExpression

errUnsupportedValue :: MonadError (TranslateError ann) m => F.Value ann -> m x
errUnsupportedValue = errUnsupportedItem . LpValue

errBadLiteral :: MonadError (TranslateError ann) m => (F.Value ann) -> m x
errBadLiteral = throwError . ErrBadLiteral

errUnexpectedType :: MonadError (TranslateError ann) m => LangPart ann -> TypeRep -> m x
errUnexpectedType lp = throwError . ErrUnexpectedType lp

errInvalidVarType :: MonadError (TranslateError ann) m => TypeRep -> m x
errInvalidVarType = throwError . ErrInvalidVarType

errInvalidOperatorApplication :: MonadError (TranslateError ann) m => LangPart ann -> m x
errInvalidOperatorApplication = throwError . ErrInvalidOperatorApplication

--------------------------------------------------------------------------------
--  Lenses
--------------------------------------------------------------------------------

makeLenses ''TranslateEnv

instance HasTypemap NumDict TranslateEnv where typemap = teNumDicts
instance HasTypemap BooleanDict TranslateEnv where typemap = teBooleanDicts
instance HasTypemap2 EqDict TranslateEnv where typemap2 = teEqDicts
instance HasTypemap2 OrdDict TranslateEnv where typemap2 = teOrdDicts

instance HasTypemap (Dict1 SymNum) TranslateEnv where typemap = teSymNumInstances
instance HasTypemap (Dict1 SymBool) TranslateEnv where typemap = teSymBoolInstances
instance HasTypemap2 (Dict2 SymEq) TranslateEnv where typemap2 = teSymEqInstances
instance HasTypemap2 (Dict2 SymOrd) TranslateEnv where typemap2 = teSymOrdInstances
