{-# LANGUAGE ConstraintKinds            #-}
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

import           Control.Exception               (Exception (..))
import           Data.Typeable                   (Proxy (..), TypeRep, Typeable,
                                                  gcast, typeRep)

import           Data.Map                        (Map)

import           Control.Lens                    hiding (op)
import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Language.Fortran.AST            as F

import           Language.Expression.Constraints
import           Language.Expression.Dict
import           Language.Expression.DSL
import           Language.Expression.Pretty
import           Language.Verification

--------------------------------------------------------------------------------
--  General types
--------------------------------------------------------------------------------

data Some p f where
  Some :: (p a, Typeable a) => f a -> Some p f

_Some :: (p a, Typeable a, p b, Typeable b) => Prism (Some p f) (Some p f) (f a) (f b)
_Some = prism Some extract
  where
    extract :: (p a, Typeable a) => Some p f -> Either (Some p f) (f a)
    extract (Some e) = maybe (Left (Some e)) Right (gcast e)

traverseSome :: Applicative m => (forall a. (p a, Typeable a) => f a -> m (g a)) -> Some p f -> m (Some p g)
traverseSome f (Some x) = Some <$> f x

mapSome :: (forall a. (p a, Typeable a) => f a -> g a) -> Some p f -> Some p g
mapSome f = runIdentity . traverseSome (Identity . f)

-- | @'Some' f p@ contains an @f a@ for some @a@. This function gets the
-- 'TypeRep' of @a@.
someTypeRep :: Some p f -> TypeRep
someTypeRep (Some x) = typeRep x

type SomeExpr = Some SymValue FortranExpr
type SomeVar l = Some Verifiable (Var l)
type SomeType = Some Verifiable (Const ())

someVarName :: Location l => SomeVar l -> String
someVarName (Some v) = varName v

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
  { _teImplictVars      :: Bool
  , _teVarsInScope      :: Map SourceName (SomeVar NamePair)

  , _teSymNumInstances  :: Dictmap SymNum
  , _teSymBoolInstances :: Dictmap SymBool
  , _teSymEqInstances   :: Dictmap2 SymEq
  , _teSymOrdInstances  :: Dictmap2 SymOrd
  }

newtype MonadTranslate ann a =
  MonadTranslate { getMonadTranslate :: ReaderT TranslateEnv (Either (TranslateError ann)) a }
  deriving (Functor, Applicative, Monad, MonadError (TranslateError ann), MonadReader TranslateEnv)

runMonadTranslate :: MonadTranslate ann a -> TranslateEnv -> Either (TranslateError ann) a
runMonadTranslate (MonadTranslate action) env = runReaderT action env

type FortranOps = CoerceOp : StandardOps

type FortranExpr = Expr' FortranOps (Var NamePair)

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
  | ErrInvalidOperatorApplication (F.Expression a) (F.Expression a) (LangPart a)
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

    ErrInvalidOperatorApplication e1 e2 lp ->
      "Tried to apply operator to arguments of the wrong type. Operator was: " ++ displayLangPart lp ++
      ". Left operand was: " ++ displayExpression e1 ++ ". Right operand was: " ++ displayExpression e2

    ErrVarNotInScope nm ->
      "Reference to variable '" ++ nm ++ "' which is not in scope."

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

errInvalidOperatorApplication :: MonadError (TranslateError ann) m => F.Expression ann -> F.Expression ann -> LangPart ann -> m x
errInvalidOperatorApplication e1 e2 lp = throwError (ErrInvalidOperatorApplication e1 e2 lp)

errVarNotInScope :: MonadError (TranslateError ann) m => F.Name -> m x
errVarNotInScope = throwError . ErrVarNotInScope

--------------------------------------------------------------------------------
--  Lenses
--------------------------------------------------------------------------------

makeLenses ''TranslateEnv

instance HasTypemap (Dict1 SymNum) TranslateEnv where typemap = teSymNumInstances
instance HasTypemap (Dict1 SymBool) TranslateEnv where typemap = teSymBoolInstances
instance HasTypemap2 (Dict2 SymEq) TranslateEnv where typemap2 = teSymEqInstances
instance HasTypemap2 (Dict2 SymOrd) TranslateEnv where typemap2 = teSymOrdInstances

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
  , _teSymBoolInstances = defaultSymBoolInstances
  , _teSymNumInstances = defaultSymNumInstances
  , _teSymEqInstances = defaultSymEqInstances
  , _teSymOrdInstances = defaultSymOrdInstances
  }

defaultSymNumInstances :: Dictmap SymNum
defaultSymNumInstances = dictmap (Proxy :: Proxy '[Integer, Double])

defaultSymBoolInstances :: Dictmap SymBool
defaultSymBoolInstances = dictmap (Proxy :: Proxy '[Bool])

defaultSymEqInstances :: Dictmap2 SymEq
defaultSymEqInstances = toDictmap2 (dictmap (Proxy :: Proxy '[Bool, Integer, Double]) :: Dictmap (SymEq Bool))

defaultSymOrdInstances :: Dictmap2 SymOrd
defaultSymOrdInstances = toDictmap2 (dictmap (Proxy :: Proxy '[Bool, Integer, Double]) :: Dictmap (SymOrd Bool))
