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
  , translateBoolExpression
  , translateFormula
  , translateTypeSpec
  , module Types
  ) where

import           Prelude                                     hiding (span)

import           Data.Char                                   (toLower)
import           Data.Maybe                                  (fromMaybe)
import           Data.Typeable
import           Text.Read                                   (readMaybe)

import           Data.SBV                                    (SymWord)
import           Data.Singletons

import           Control.Lens                                hiding (op, (.>))

import qualified Language.Fortran.AST                        as F

import           Language.Expression.DSL
import           Language.Fortran.TypeModel
import           Language.Fortran.TypeModel.DSL
import           Language.Fortran.TypeModel.Machinery
import           Language.Fortran.TypeModel.SBV

import           Camfort.Specification.Hoare.Syntax
import           Camfort.Specification.Hoare.Translate.Types as Types

--------------------------------------------------------------------------------
--  Translate
--------------------------------------------------------------------------------

translateTypeSpec :: F.TypeSpec ann -> MonadTranslate ann SomeType
translateTypeSpec = \case
  -- TODO: Get precision right
  ts@(F.TypeSpec _ _ bt Nothing) -> case bt of
    F.TypeInteger         -> return $ someType DInt64
    F.TypeReal            -> return $ someType DFloat
    F.TypeDoublePrecision -> return $ someType DDouble
    F.TypeLogical         -> return $ someType DBool8
    _                     -> errUnsupportedTypeSpec ts
  ts@_ -> errUnsupportedTypeSpec ts

translateFormula :: PrimFormula ann -> MonadTranslate ann (TransFormula Bool)
translateFormula = \case
  PFExpr e -> do
    e' <- translateBoolExpression e
    return (expr e')

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

translateBoolExpression :: F.Expression ann -> MonadTranslate ann (FortranExpr Bool)
translateBoolExpression e = do
  Some e' <- translateExpression e

  case matchKind (dForType e') of
    MKLogical -> return (asProp e')
    _ -> errUnexpectedType (LpExpression e) (typeRep (Proxy :: Proxy Bool)) (typeRep e')

translateExpression'
  :: (HasRepr r, Typeable r) => F.Expression ann -> MonadTranslate ann (FortranExpr r)
translateExpression' = translateAtType LpExpression translateExpression

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
  v@(F.ValInteger s) -> translateLiteral v (readMaybe :: String -> Maybe Int64) s

  v@(F.ValReal s) -> translateLiteral v (readMaybe :: String -> Maybe Double) s

  v@(F.ValComplex realPart complexPart) -> errUnsupportedValue v
  v@(F.ValString s) -> errUnsupportedValue v
  v@(F.ValHollerith s) -> errUnsupportedValue v

  v@(F.ValVariable nm) -> do
    theVar <- view (teVarsInScope . at (SourceName nm))
    case theVar of
      Just (Some v') -> return (Some (var v'))
      _              -> errVarNotInScope nm

  v@(F.ValIntrinsic nm) -> errUnsupportedValue v

  v@(F.ValLogical s) ->
    let intoBool l = case map toLower l of
          ".true."  -> Just (Bool8 true)
          ".false." -> Just (Bool8 false)
          _         -> Nothing
    in translateLiteral v intoBool s

  v@(F.ValOperator s) -> errUnsupportedValue v
  v@(F.ValAssignment) -> errUnsupportedValue v
  v@(F.ValType s) -> errUnsupportedValue v
  v@(F.ValStar) -> errUnsupportedValue v


translateLiteral
  :: (HasRepr a, SymWord a, Typeable a)
  => F.Value ann -> (s -> Maybe a) -> s -> MonadTranslate ann SomeExpr
translateLiteral v readLit
  = fromMaybe (errBadLiteral v)
  . fmap (return . Some . lit)
  . readLit


data SomeOp f where
  SomeOp :: SingI (ok :: OpKind) => f ok -> SomeOp f

getUopOp :: F.UnaryOp -> Maybe (SomeOp Op1)
getUopOp = \case
  F.Minus -> Just (SomeOp OpNeg)
  F.Plus -> Just (SomeOp OpPos)
  F.Not -> Just (SomeOp OpNot)
  _ -> Nothing

getBopOp :: F.BinaryOp -> Maybe (SomeOp Op2)
getBopOp = \case
  F.Addition -> Just (SomeOp OpAdd)
  F.Subtraction -> Just (SomeOp OpSub)
  F.Multiplication -> Just (SomeOp OpMul)
  F.Division -> Just (SomeOp OpDiv)

  F.LT -> Just (SomeOp OpLT)
  F.GT -> Just (SomeOp OpGT)
  F.LTE -> Just (SomeOp OpLE)
  F.GTE -> Just (SomeOp OpGE)

  F.EQ -> Just (SomeOp OpEq)
  F.NE -> Just (SomeOp OpNE)

  F.And -> Just (SomeOp OpAnd)
  F.Or -> Just (SomeOp OpOr)
  F.Equivalent -> Just (SomeOp OpEquiv)
  F.NotEquivalent -> Just (SomeOp OpNotEquiv)

  _ -> Nothing


translateBop :: F.Expression ann -> F.Expression ann -> F.BinaryOp -> MonadTranslate ann SomeExpr
translateBop e1 e2 bop = do

  SomeOp realOp <- case getBopOp bop of
    Just x  -> return x
    Nothing -> errUnsupportedItem (LpBinaryOp bop)

  Some e1' <- translateExpression e1
  Some e2' <- translateExpression e2

  let d1 = dForType e1'
      d2 = dForType e2'

  SomeOp2Result opResult <- case getOp2Result (singByProxy realOp) d1 d2 of
    Just x -> return x
    Nothing -> errInvalidBinopApplication (e1, typeRep e1') (e2, typeRep e2') (LpBinaryOp bop)

  DWithParams d3 <- case getDFromOp2Result opResult of
    Just x -> return x
    Nothing -> errInvalidBinopApplication (e1, typeRep e1') (e2, typeRep e2') (LpBinaryOp bop)

  let res = Op2 realOp opResult d1 d2 d3 e1' e2'

  return (Some (EOp res))


translateUop :: F.Expression ann -> F.UnaryOp -> MonadTranslate ann SomeExpr
translateUop e uop = do

  SomeOp realOp <- case getUopOp uop of
    Just x -> return x
    Nothing -> errUnsupportedItem (LpUnaryOp uop)

  Some e' <- translateExpression e

  let d = dForType e'

  SomeOp1Result opResult <- case getOp1Result (singByProxy realOp) d of
    Just x -> return x
    Nothing -> errInvalidUnopApplication (e, typeRep e') (LpUnaryOp uop)

  DWithParams dr <- case getDFromOp1Result opResult of
    Just x -> return x
    Nothing -> errInvalidUnopApplication (e, typeRep e') (LpUnaryOp uop)

  let res = Op1 realOp opResult d dr e'

  return (Some (EOp res))


--------------------------------------------------------------------------------
--  Dynamically typed expressions
--------------------------------------------------------------------------------


translateAtType
  :: forall r a ann. (HasRepr r, Typeable r)
  => (a -> LangPart ann)
  -> (a -> MonadTranslate ann SomeExpr)
  -> a -> MonadTranslate ann (FortranExpr r)
translateAtType toLp translate x =
  do Some someY <- translate x
     case gcast someY of
       Just y  -> return y
       Nothing -> errUnexpectedType (toLp x) (typeRep (Proxy :: Proxy r)) (typeRep someY)
