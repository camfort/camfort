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
import           Text.Read                                   (readMaybe)

import           Control.Lens                                hiding (op, (.>))

import           Data.Vinyl

import qualified Language.Fortran.AST                        as F

import           Language.Expression.DSL
import           Language.Fortran.TypeModel
import           Language.Fortran.TypeModel.Vars
import           Language.Fortran.TypeModel.Match
import           Language.Fortran.TypeModel.Singletons

import           Camfort.Specification.Hoare.Syntax
import           Camfort.Specification.Hoare.Translate.Types as Types

--------------------------------------------------------------------------------
--  Translate
--------------------------------------------------------------------------------

translateTypeSpec :: F.TypeSpec ann -> MonadTranslate ann SomeType
translateTypeSpec = \case
  -- TODO: Get precision right
  -- TODO: Arrays (consider selectors)
  -- TODO: Derived data types (consider F.TypeCustom)
  ts@(F.TypeSpec _ _ bt Nothing) -> case bt of
    F.TypeInteger         -> return $ someType (DPrim PInt64)
    F.TypeReal            -> return $ someType (DPrim PFloat)
    F.TypeDoublePrecision -> return $ someType (DPrim PDouble)
    F.TypeCharacter       -> return $ someType (DPrim PChar)
    F.TypeLogical         -> return $ someType (DPrim PBool8)
    _                     -> errUnsupportedTypeSpec ts
  ts@_ -> errUnsupportedTypeSpec ts


translateFormula :: PrimFormula ann -> MonadTranslate ann (TransFormula Bool)
translateFormula = \case
  PFExpr e -> do
    e' <- translateBoolExpression e
    return $ expr $ e'

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


translateBoolExpression
  :: F.Expression ann
  -> MonadTranslate ann (FExpr FortranVar Bool)
translateBoolExpression e = do
  Some d1 e' <- translateExpression e

  resUnsquashed :: Expr FLiftLogical FortranExpr Bool <- case matchPrim d1 of
    Just (MatchPrim _ SKLogical prim1) -> return $ EOp $
      case prim1 of
        PBool8  -> FLL8 (EVar e')
        PBool16 -> FLL16 (EVar e')
        PBool32 -> FLL32 (EVar e')
        PBool64 -> FLL64 (EVar e')
    _ -> errUnexpectedType (LpExpression e) (someType (DPrim PBool8)) (someType d1)

  return (squashExpression resUnsquashed)


translateExpression'
  :: D a -> F.Expression ann
  -> MonadTranslate ann (FortranExpr a)
translateExpression' d = translateAtType LpExpression d translateExpression


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
  v@(F.ValInteger s) -> translateLiteral v PInt64 readMaybe s

  v@(F.ValReal s) -> translateLiteral v PDouble readMaybe s

  v@(F.ValComplex realPart complexPart) -> errUnsupportedValue v
  v@(F.ValString s) -> errUnsupportedValue v
  v@(F.ValHollerith s) -> errUnsupportedValue v

  -- TODO: Auxiliary variables
  v@(F.ValVariable nm) -> do
    theVar <- view (teVarsInScope . at (SourceName nm))
    case theVar of
      Just (Some d v') -> return (Some d (EVar v'))
      _                -> errVarNotInScope nm

  v@(F.ValIntrinsic nm) -> errUnsupportedValue v

  v@(F.ValLogical s) ->
    let intoBool l = case map toLower l of
          ".true."  -> Just (Bool8 1)
          ".false." -> Just (Bool8 0)
          _         -> Nothing
    in translateLiteral v PBool8 intoBool s

  v@(F.ValOperator s) -> errUnsupportedValue v
  v@F.ValAssignment -> errUnsupportedValue v
  v@(F.ValType s) -> errUnsupportedValue v
  v@F.ValStar -> errUnsupportedValue v


translateLiteral
  :: F.Value ann
  -> Prim p k (PrimS a) -> (s -> Maybe a) -> s
  -> MonadTranslate ann SomeExpr
translateLiteral v pa readLit
  = maybe (errBadLiteral v) (return . Some (DPrim pa) . flit pa)
  . readLit
  where
    flit px x = EOp (FortranOp OpLit (ORLit px x) RNil)


data SomeOp (f :: OpKind -> *) where
  SomeOp :: f ok -> SomeOp f

getUopOp :: F.UnaryOp -> Maybe (SomeOp (Op 1))
getUopOp = \case
  F.Minus -> Just (SomeOp OpNeg)
  F.Plus -> Just (SomeOp OpPos)
  F.Not -> Just (SomeOp OpNot)
  _ -> Nothing

getBopOp :: F.BinaryOp -> Maybe (SomeOp (Op 2))
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
  SomeOp bop' <- case getBopOp bop of
    Just x  -> return x
    Nothing -> errUnsupportedItem (LpBinaryOp bop)

  Some d1 e1' <- translateExpression e1
  Some d2 e2' <- translateExpression e2

  MatchOpR opResult d3 <- case matchOpR bop' (d1 :& d2 :& RNil) of
    Just x -> return x
    Nothing -> errInvalidBinopApplication (e1, someType d1) (e2, someType d2) (LpBinaryOp bop)

  return $ Some d3 $ EOp $ FortranOp bop' opResult (e1' :& e2' :& RNil)


translateUop :: F.Expression ann -> F.UnaryOp -> MonadTranslate ann SomeExpr
translateUop e uop = do
  SomeOp uop' <- case getUopOp uop of
    Just x  -> return x
    Nothing -> errUnsupportedItem (LpUnaryOp uop)

  Some d1 e' <- translateExpression e

  MatchOpR opResult d2 <- case matchOpR uop' (d1 :& RNil) of
    Just x  -> return x
    Nothing -> errInvalidUnopApplication (e, someType d1) (LpUnaryOp uop)

  return $ Some d2 $ EOp $ FortranOp uop' opResult (e' :& RNil)


--------------------------------------------------------------------------------
--  Dynamically typed expressions
--------------------------------------------------------------------------------

translateAtType
  :: (a -> LangPart ann)
  -> D b
  -> (a -> MonadTranslate ann SomeExpr)
  -> a -> MonadTranslate ann (FortranExpr b)
translateAtType toLp db translate x =
  do Some da someY <- translate x
     case dcast da db someY of
       Just y  -> return y
       Nothing -> errUnexpectedType (toLp x) (someType da) (someType db)
