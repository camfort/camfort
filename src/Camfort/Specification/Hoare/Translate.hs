{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- TODO: Implement translation for more unsupported language parts

module Camfort.Specification.Hoare.Translate
  ( translateExpression
  , translateExpression'
  , translateFormula
  , translateTypeSpec
  , module Types
  ) where

import           Prelude                                     hiding (span)

import           Data.Char                                   (toLower)
import           Data.Maybe                                  (fromMaybe,
                                                              listToMaybe)
import           Data.Typeable
import           Text.Read                                   (readMaybe)

import           Control.Lens                                hiding (op, (.>))

import qualified Language.Fortran.AST                        as F

import           Language.Expression.DSL
import           Language.Expression.Constraints
import           Language.Expression.Dict

import           Camfort.Specification.Hoare.Syntax
import           Camfort.Specification.Hoare.Translate.Types as Types

--------------------------------------------------------------------------------
--  Translate
--------------------------------------------------------------------------------

translateTypeSpec :: F.TypeSpec ann -> MonadTranslate ann SomeType
translateTypeSpec = \case
  ts@(F.TypeSpec _ _ bt Nothing) -> case bt of
    F.TypeInteger         -> return $ Some (Const () :: Const () Integer)
    F.TypeReal            -> return $ Some (Const () :: Const () Float)
    F.TypeDoublePrecision -> return $ Some (Const () :: Const () Double)
    F.TypeLogical         -> return $ Some (Const () :: Const () Bool)
    _ -> errUnsupportedTypeSpec ts
  ts@_ -> errUnsupportedTypeSpec ts

translateFormula :: PrimFormula ann -> MonadTranslate ann (TransFormula Bool)
translateFormula = \case
  PFExpr e -> expr <$> translateExpression' e
  PFCompare x -> translateCompare <$> traverse translateExpression' x
  PFLogical x -> translateLogical <$> traverse translateFormula x

translateExpression :: F.Expression ann -> MonadTranslate ann SomeExpr
translateExpression = \case
  e@(F.ExpValue ann span val) -> translateValue val
  e@(F.ExpBinary ann span bop e1 e2) -> translateBop e1 e2 bop
  e@(F.ExpUnary ann span uop operand) -> translateUop operand uop

  e@(F.ExpSubscript ann span lhs indices') -> errUnsupportedExpression e
  e@(F.ExpDataRef ann span e1 e2) -> errUnsupportedExpression e
  e@(F.ExpFunctionCall ann span fexpr args) -> errUnsupportedExpression e
  e@(F.ExpImpliedDo ann span es spec) -> errUnsupportedExpression e
  e@(F.ExpInitialisation ann span es) -> errUnsupportedExpression e
  e@(F.ExpReturnSpec ann span rval) -> errUnsupportedExpression e

translateExpression' :: (SymValue r) => F.Expression ann -> MonadTranslate ann (FortranExpr r)
translateExpression' = translateAtType LpExpression translateExpression

-- TODO: Allow this to deal with expressions that aren't integer-valued

translateCompare :: PrimComp (FortranExpr Integer) -> TransFormula Bool
translateCompare = \case
  PCLess x y -> expr $ x .< y
  PCLessEq x y -> expr $ x .<= y
  PCGreater x y -> expr $ x .> y
  PCGreaterEq x y -> expr $ x .>= y
  PCEq x y -> expr $ x .== y
  PCNeq x y -> expr $ x ./= y

translateLogical :: PrimLogic (TransFormula Bool) -> TransFormula Bool
translateLogical = \case
  PLAnd x y -> x *&& y
  PLOr x y -> x *|| y
  PLImpl x y -> x *-> y
  PLEquiv x y -> x *<-> y
  PLNot x -> pnot x
  PLLit x -> plit x


translateValue :: F.Value ann -> MonadTranslate ann SomeExpr
translateValue = \case
  v@(F.ValInteger s) -> translateLiteral v (readMaybe :: String -> Maybe Integer) s

  v@(F.ValReal s) -> translateLiteral v (readMaybe :: String -> Maybe Double) s

  v@(F.ValComplex realPart complexPart) -> errUnsupportedValue v
  v@(F.ValString s) -> errUnsupportedValue v
  v@(F.ValHollerith s) -> errUnsupportedValue v

  v@(F.ValVariable nm) -> do
    theVar <- view (teVarsInScope . at (SourceName nm))
    case theVar of
      Just (Some v') -> return (Some (var v'))
      _ -> errVarNotInScope nm

  v@(F.ValIntrinsic nm) -> errUnsupportedValue v

  v@(F.ValLogical s) ->
    let intoBool l = case map toLower l of
          ".true." -> Just True
          ".false." -> Just False
          _ -> Nothing
    in translateLiteral v intoBool s

  v@(F.ValOperator s) -> errUnsupportedValue v
  v@(F.ValAssignment) -> errUnsupportedValue v
  v@(F.ValType s) -> errUnsupportedValue v
  v@(F.ValStar) -> errUnsupportedValue v


translateLiteral :: (SymLit a) => F.Value ann -> (s -> Maybe a) -> s -> MonadTranslate ann SomeExpr
translateLiteral v readLit = fromMaybe (errBadLiteral v) . fmap (return . Some . elit) . readLit


translateBop :: F.Expression ann -> F.Expression ann -> F.BinaryOp -> MonadTranslate ann SomeExpr
translateBop e1 e2 op = case op of
  F.Addition       -> numericBop op (.+) e1 e2
  F.Subtraction    -> numericBop op (.-) e1 e2
  F.Multiplication -> numericBop op (.*) e1 e2

  F.LT  -> orderingBop op (.<) e1 e2
  F.LTE -> orderingBop op (.<=) e1 e2
  F.GT  -> orderingBop op (.>) e1 e2
  F.GTE -> orderingBop op (.>=) e1 e2

  F.EQ -> equalityBop op (.==) e1 e2
  F.NE -> equalityBop op (./=) e1 e2

  F.And -> booleanBop op (.&&) e1 e2
  F.Or  -> booleanBop op (.||) e1 e2

  _ -> errUnsupportedItem (LpBinaryOp op)


translateUop :: F.Expression ann -> F.UnaryOp -> MonadTranslate ann SomeExpr
translateUop e = \case
  F.Not -> do
    e' :: FortranExpr Bool <- translateExpression' e
    return (Some (enot e'))

  op@_ -> errUnsupportedItem (LpUnaryOp op)

--------------------------------------------------------------------------------
--  Operators
--------------------------------------------------------------------------------

numericBop
  :: F.BinaryOp
  -> (forall a. SymNum a => FortranExpr a -> FortranExpr a -> FortranExpr a)
  -> F.Expression ann -> F.Expression ann
  -> MonadTranslate ann SomeExpr
numericBop op f e1 e2 = do
  numInstances :: Dictmap SymNum <- view typemap

  Some e1' <- translateExpression e1

  fromMaybe (errInvalidOperatorApplication e1 e2 (LpBinaryOp op)) $
    withDictmap numInstances e1' $ do
      e2' <- translateExpression' e2
      return (Some (f e1' e2'))


booleanBop
  :: F.BinaryOp
  -> (forall a. SymBool a => FortranExpr a -> FortranExpr a -> FortranExpr a)
  -> F.Expression ann -> F.Expression ann
  -> MonadTranslate ann SomeExpr
booleanBop op f e1 e2 = do
  boolInstances :: Dictmap SymBool <- view typemap

  Some e1' <- translateExpression e1

  fromMaybe (errInvalidOperatorApplication e1 e2 (LpBinaryOp op)) $
    withDictmap boolInstances e1' $ do
      e2' <- translateExpression' e2
      return (Some (f e1' e2'))


equalityBop
  :: F.BinaryOp
  -> (forall b a. SymEq b a => FortranExpr a -> FortranExpr a -> FortranExpr b)
  -> F.Expression ann -> F.Expression ann
  -> MonadTranslate ann SomeExpr
equalityBop op f e1 e2 = do
  eqInstances :: Dictmap2 SymEq <- view typemap2
  Some e1' <- translateExpression e1

  fromMaybe (errInvalidOperatorApplication e1 e2 (LpBinaryOp op)) . listToMaybe $
    withDictmap2' eqInstances e1' $ \(_ :: Proxy b) -> do
      e2' <- translateExpression' e2
      return (Some (f e1' e2' :: FortranExpr b))


orderingBop
  :: F.BinaryOp
  -> (forall b a. SymOrd b a => FortranExpr a -> FortranExpr a -> FortranExpr b)
  -> F.Expression ann -> F.Expression ann
  -> MonadTranslate ann SomeExpr
orderingBop op f e1 e2 = do
  ordInstances :: Dictmap2 SymOrd <- view typemap2
  Some e1' <- translateExpression e1

  fromMaybe (errInvalidOperatorApplication e1 e2 (LpBinaryOp op)) . listToMaybe $
    withDictmap2' ordInstances e1' $ \(_ :: Proxy b) -> do
      e2' <- translateExpression' e2
      return (Some (f e1' e2' :: FortranExpr b))

--------------------------------------------------------------------------------
--  Dynamically typed expressions
--------------------------------------------------------------------------------

-- TODO: Check if the types are actually coercible in Fortran.
tryCoerce :: (SymValue a, SymValue b) => FortranExpr a -> Maybe (FortranExpr b)
tryCoerce e = Just (ecoerce e)


-- | Given a dynamically typed expression, extract the underlying typed
-- expression. If it is already the desired type, return it as is. Otherwise,
-- try to coerce it to the desired type. Returns the 'TypeRep' of the
-- expression's real type, if the value is the wrong type and cannot be coerced
-- into the correct type.
extractOrCoerceExpr :: forall a. (SymValue a) => SomeExpr -> Either TypeRep (FortranExpr a)
extractOrCoerceExpr (Some (e :: FortranExpr b))
  | Just Refl <- eqT :: Maybe (a :~: b) = Right e
  | Just x <- tryCoerce e = Right x
  | otherwise = Left (typeRep (Proxy :: Proxy b))


translateAtType
  :: (SymValue r)
  => (a -> LangPart ann)
  -> (a -> MonadTranslate ann SomeExpr)
  -> a -> MonadTranslate ann (FortranExpr r)
translateAtType toLp translate x =
  do someY <- translate x
     case extractOrCoerceExpr someY of
       Right y -> return y
       Left ty -> errUnexpectedType (toLp x) ty
