{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- TODO: Expand range of language parts that can be translated

module Camfort.Specification.Hoare.Translation where

import           Data.Typeable                              ((:~:) (..),
                                                             Proxy (..),
                                                             TypeRep, Typeable,
                                                             eqT, typeRep)
import           Text.Read                                  (readMaybe)
import           Data.Char                                  (toLower)

import           Control.Monad.Except
import           Control.Lens

import qualified Language.Fortran.AST                       as F

import           Language.Verification

--------------------------------------------------------------------------------
--  Translation Monad
--------------------------------------------------------------------------------

type MonadTranslate ann a = Either (TranslationError ann) a

type FortranOps = CoerceOp : StandardOps

type FortranExpr = Expr' FortranOps (Var String)

data SomeExpr where
  SomeExpr :: Typeable a => FortranExpr a -> SomeExpr



--------------------------------------------------------------------------------
--  Errors
--------------------------------------------------------------------------------

data TranslationError a
  = ErrUnsupportedItem (LangPart a)
  -- ^ Tried to translate a part of the language that is not (yet) supported.
  | ErrBadLiteral (F.Value a)
  -- ^ Found a literal value that we didn't know how to translate. May or may
  -- not be valid Fortran.
  | ErrUnexpectedType (LangPart a) TypeRep
  -- ^ Tried to translate a FORTRAN language part into the wrong expression
  -- type.
  | ErrInvalidVarType TypeRep
  -- ^ Tried to make a variable representing a value of a type that can't be
  -- stored in a variable.

data LangPart a
  = LpExpression (F.Expression a)
  | LpValue (F.Value a)

errUnsupportedExpression :: MonadError (TranslationError ann) m => F.Expression ann -> m x
errUnsupportedExpression = throwError . ErrUnsupportedItem . LpExpression

errUnsupportedValue :: MonadError (TranslationError ann) m => F.Value ann -> m x
errUnsupportedValue = throwError . ErrUnsupportedItem . LpValue

errBadLiteral :: MonadError (TranslationError ann) m => (F.Value ann) -> m x
errBadLiteral = throwError . ErrBadLiteral

errUnexpectedType :: MonadError (TranslationError ann) m => LangPart ann -> TypeRep -> m x
errUnexpectedType lp = throwError . ErrUnexpectedType lp

errInvalidVarType :: MonadError (TranslationError ann) m => TypeRep -> m x
errInvalidVarType = throwError . ErrInvalidVarType

--------------------------------------------------------------------------------
--  Translation
--------------------------------------------------------------------------------

translateLiteral
  :: (SymLit a, Typeable r)
  => F.Value ann
  -> (s -> Maybe a)
  -> s
  -> MonadTranslate ann (FortranExpr r)
translateLiteral v readLit s = do
  do x <- tryMaybe (errBadLiteral v) (readLit s)
     tryTypeable (LpValue v) (lit x)

translateValue :: (Typeable r) => F.Value ann -> MonadTranslate ann (FortranExpr r)
translateValue = \case
  v@(F.ValInteger s) -> translateLiteral v (readMaybe :: String -> Maybe Integer) s

  v@(F.ValReal s) -> translateLiteral v (readMaybe :: String -> Maybe Double) s

  v@(F.ValComplex realPart complexPart) -> errUnsupportedValue v
  v@(F.ValString s) -> errUnsupportedValue v
  v@(F.ValHollerith s) -> errUnsupportedValue v

  v@(F.ValVariable nm) -> var <$> tryMakeVar nm

  v@(F.ValIntrinsic nm) -> errUnsupportedValue v

  v@(F.ValLogical s) ->
    let intoBool s = case map toLower s of
          ".true." -> Just True
          ".false." -> Just False
          _ -> Nothing
    in translateLiteral v intoBool s

  v@(F.ValOperator s) -> errUnsupportedValue v
  v@(F.ValAssignment) -> errUnsupportedValue v
  v@(F.ValType s) -> errUnsupportedValue v
  v@(F.ValStar) -> errUnsupportedValue v


translateExpression :: Typeable r => F.Expression ann -> MonadTranslate ann (FortranExpr r)
translateExpression = \case
  e@(F.ExpValue ann span val) -> translateValue val

  e@(F.ExpBinary ann span bop e1 e2) -> translateBop e1 e2 bop

  e@(F.ExpUnary ann span uop operand) -> translateUop operand uop

  e@(F.ExpSubscript ann span lhs indices) -> errUnsupportedExpression e
  e@(F.ExpDataRef ann span e1 e2) -> errUnsupportedExpression e
  e@(F.ExpFunctionCall ann span fexpr args) -> errUnsupportedExpression e
  e@(F.ExpImpliedDo ann span es spec) -> errUnsupportedExpression e
  e@(F.ExpInitialisation ann span es) -> errUnsupportedExpression e
  e@(F.ExpReturnSpec ann span rval) -> errUnsupportedExpression e


translateBop
  :: (Typeable r)
  => F.Expression ann
  -> F.Expression ann
  -> F.BinaryOp
  -> MonadTranslate ann (FortranExpr r)

translateBop e1 e2 = error "translateBop"

translateUop
  :: (Typeable r)
  => F.Expression ann
  -> F.UnaryOp
  -> MonadTranslate ann (FortranExpr r)

translateUop e1 e2 = error "translateUop"

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------

tryTypeable
  :: forall a b c ann m. (Typeable a, Typeable b,
                        MonadError (TranslationError ann) m)
  => LangPart ann
  -> c a
  -> m (c b)
tryTypeable lp x =
  case eqT :: Maybe (a :~: b) of
    Just Refl -> return x
    Nothing -> errUnexpectedType lp (typeRep (Proxy :: Proxy a))


tryMaybe :: (MonadError e m) => m a -> Maybe a -> m a
tryMaybe err = maybe err return


tryMakeVar :: forall a l ann m. (Typeable a, Location l, MonadError (TranslationError ann) m) => l -> m (Var l a)
tryMakeVar loc
  | Just Refl <- eqT :: Maybe (a :~: Integer) = return (Var loc)
  | otherwise = undefined
