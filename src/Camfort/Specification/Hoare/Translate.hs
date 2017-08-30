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
{-# LANGUAGE OverloadedStrings     #-}

-- TODO: Implement translation for more unsupported language parts

module Camfort.Specification.Hoare.Translate
  ( fortranToFExpr
  , translateExpression
  , translateExpression'
  , translateBoolExpression
  , translateFormula

  , translateTypeInfo
  , TypeInfo
  , typeInfo
  , tiWithDimensionDecls
  , tiWithAttributes
  , tiWithDeclLength

  , module Types
  ) where

import           Prelude                                     hiding (span)

import           Data.Char                                   (toLower)
import           Text.Read                                   (readMaybe)
import           Data.Maybe (catMaybes)
import           Control.Applicative ((<|>))

import           Control.Lens                                hiding (op, (.>), rmap)
import Control.Monad.Except (throwError)

import           Data.Singletons.Prelude.List                (Length)
import           Data.Vinyl

import qualified Language.Fortran.Util.Position              as F
import qualified Language.Fortran.AST                        as F

import           Language.Expression.DSL
import           Language.Fortran.TypeModel
import           Language.Fortran.TypeModel.Match
import           Language.Fortran.TypeModel.Singletons
import           Language.Fortran.TypeModel.Vars
import           Camfort.Analysis.Logger

import           Camfort.Specification.Hoare.Syntax
import           Camfort.Specification.Hoare.Translate.Types as Types

--------------------------------------------------------------------------------
--  Util
--------------------------------------------------------------------------------

fortranToFExpr :: FortranExpr a -> FExpr FortranVar a
fortranToFExpr (e :: FortranExpr a) =
  let e' :: Expr FLiftLogical (Expr FortranOp FortranVar) a
      e' = EVar e
  in squashExpression e'

--------------------------------------------------------------------------------
--  Translate
--------------------------------------------------------------------------------

data SomePrimD where
  SomePrimD :: D (PrimS a) -> SomePrimD

translateBaseType
  :: F.BaseType
  -> Maybe (F.Expression ann) -- ^ Kind
  -> Maybe SomePrimD
translateBaseType bt Nothing = case bt of
  F.TypeInteger         -> Just $ SomePrimD (DPrim PInt64)
  F.TypeReal            -> Just $ SomePrimD (DPrim PFloat)
  F.TypeDoublePrecision -> Just $ SomePrimD (DPrim PDouble)
  F.TypeCharacter       -> Just $ SomePrimD (DPrim PChar)
  F.TypeLogical         -> Just $ SomePrimD (DPrim PBool8)
  _                     -> Nothing
translateBaseType _ _ = Nothing -- TODO: Support kind specifiers


translateTypeInfo
  :: TypeInfo ann
  -> MonadTranslate SomeType
translateTypeInfo ti = do
  let mtranslateBase = translateBaseType (tiBaseType ti) (tiSelectorKind ti)

  SomePrimD basePrim <-
    maybe (throwError $ ErrUnsupportedItem "type spec") return mtranslateBase

  let
    -- If an attribute corresponds to a dimension declaration which contains a
    -- simple length dimension, get the expression out.
    attrToLength (F.AttrDimension _ _ decls) = dimDeclsToLength decls
    attrToLength _ = Nothing

    attrsToLength (F.AList _ _ attrs) =
      case catMaybes (attrToLength <$> attrs) of
        [e] -> Just e
        _ -> Nothing

    -- If a list of dimension declarators corresponds to a simple length
    -- dimension, get the expression out. We don't handle other cases yet.
    dimDeclsToLength (F.AList _ _ [F.DimensionDeclarator _ _ (Just e) Nothing]) = Just e
    dimDeclsToLength _ = Nothing

    mLengthExp =
      (tiSelectorLength ti) <|>
      (tiDeclLength ti) <|>
      (tiDimensionDecls ti >>= dimDeclsToLength) <|>
      (tiAttributes ti >>= attrsToLength)

  case mLengthExp of
    Just lengthExp -> do
      -- If a length expression could be found, this variable is an array

      -- TODO: If the length expression is malformed, throw an error.
      -- TODO: Use information about the length.
      -- maybe (throwError $ ErrUnsupportedItem "type spec") void (exprIntLit lengthExp)
      case basePrim of
        DPrim bp -> return (Some (DArray (Index PInt64) (ArrValue bp)))
    Nothing ->
      return (Some basePrim)


translateFormula :: PrimFormula ann -> MonadTranslate (TransFormula Bool)
translateFormula = \case
  PFExpr e -> do
    e' <- translateBoolExpression e
    return $ expr $ e'

  PFLogical x -> translateLogical <$> traverse translateFormula x


translateExpression :: F.Expression ann -> MonadTranslate SomeExpr
translateExpression = \case
  e@(F.ExpValue ann span val) -> translateValue val
  e@(F.ExpBinary ann span bop e1 e2) -> translateOp2App e1 e2 bop
  e@(F.ExpUnary ann span uop operand) -> translateOp1App operand uop

  e@(F.ExpSubscript ann span lhs indices')  -> unsupported
  e@(F.ExpDataRef ann span e1 e2)           -> unsupported
  e@(F.ExpFunctionCall ann span fexpr args) -> unsupported
  e@(F.ExpImpliedDo ann span es spec)       -> unsupported
  e@(F.ExpInitialisation ann span es)       -> unsupported
  e@(F.ExpReturnSpec ann span rval)         -> unsupported
  where unsupported = throwError $ ErrUnsupportedItem "expression"


translateBoolExpression
  :: F.Expression ann
  -> MonadTranslate (FExpr FortranVar Bool)
translateBoolExpression e = do
  SomePair d1 e' <- translateExpression e

  resUnsquashed :: Expr FLiftLogical FortranExpr Bool <- case matchPrimD d1 of
    Just (MatchPrimD (MatchPrim _ SKLogical) prim1) -> return $ EOp $
      case prim1 of
        PBool8  -> FLL8 (EVar e')
        PBool16 -> FLL16 (EVar e')
        PBool32 -> FLL32 (EVar e')
        PBool64 -> FLL64 (EVar e')
    _ -> throwError $ ErrUnexpectedType "formula" (Some (DPrim PBool8)) (Some d1)

  return (squashExpression resUnsquashed)


translateExpression'
  :: D a -> F.Expression ann
  -> MonadTranslate (FortranExpr a)
translateExpression' d = translateAtType "expression" d translateExpression


translateLogical :: PrimLogic (TransFormula Bool) -> TransFormula Bool
translateLogical = \case
  PLAnd x y -> x *&& y
  PLOr x y -> x *|| y
  PLImpl x y -> x *-> y
  PLEquiv x y -> x *<-> y
  PLNot x -> pnot x
  PLLit x -> plit x


translateValue :: F.Value ann -> MonadTranslate SomeExpr
translateValue = \case
  v@(F.ValInteger s) -> translateLiteral v PInt64 (fmap fromIntegral . readLitInteger) s

  v@(F.ValReal s) -> translateLiteral v PFloat (fmap realToFrac . readLitReal) s

  v@(F.ValComplex realPart complexPart) -> throwError $ ErrUnsupportedItem "complex literal"
  v@(F.ValString s) -> throwError $ ErrUnsupportedItem "string literal"
  v@(F.ValHollerith s) -> throwError $ ErrUnsupportedItem "hollerith literal"

  -- TODO: Auxiliary variables
  v@(F.ValVariable nm) -> do
    theVar <- view (teVarsInScope . at (SourceName nm))
    case theVar of
      Just (Some v'@(FortranVar d _)) -> return (SomePair d (EVar v'))
      _                               -> throwError $ ErrVarNotInScope nm

  v@(F.ValIntrinsic nm) -> throwError $ ErrUnsupportedItem $ "intrinsic " <> describe nm

  v@(F.ValLogical s) ->
    let intoBool = fmap (\b -> if b then Bool8 1 else Bool8 0) . readLitBool
    in translateLiteral v PBool8 intoBool s

  v@(F.ValOperator s) -> throwError $ ErrUnsupportedItem "user-defined operator"
  v@F.ValAssignment -> throwError $ ErrUnsupportedItem "interface assignment"
  v@(F.ValType s) -> throwError $ ErrUnsupportedItem "type value"
  v@F.ValStar -> throwError $ ErrUnsupportedItem "star value"


translateLiteral
  :: F.Value ann
  -> Prim p k a -> (s -> Maybe a) -> s
  -> MonadTranslate SomeExpr
translateLiteral v pa readLit
  = maybe (throwError ErrBadLiteral) (return . SomePair (DPrim pa) . flit pa)
  . readLit
  where
    flit px x = EOp (FortranOp OpLit (ORLit px x) RNil)


translateOp1 :: F.UnaryOp -> Maybe (Some (Op 1))
translateOp1 = \case
  F.Minus -> Just (Some OpNeg)
  F.Plus -> Just (Some OpPos)
  F.Not -> Just (Some OpNot)
  _ -> Nothing


translateOp2 :: F.BinaryOp -> Maybe (Some (Op 2))
translateOp2 = \case
  F.Addition -> Just (Some OpAdd)
  F.Subtraction -> Just (Some OpSub)
  F.Multiplication -> Just (Some OpMul)
  F.Division -> Just (Some OpDiv)

  F.LT -> Just (Some OpLT)
  F.GT -> Just (Some OpGT)
  F.LTE -> Just (Some OpLE)
  F.GTE -> Just (Some OpGE)

  F.EQ -> Just (Some OpEq)
  F.NE -> Just (Some OpNE)

  F.And -> Just (Some OpAnd)
  F.Or -> Just (Some OpOr)
  F.Equivalent -> Just (Some OpEquiv)
  F.NotEquivalent -> Just (Some OpNotEquiv)

  _ -> Nothing


data SameLength as bs where
  SameLength :: Length as ~ Length bs => SameLength as bs

recSequenceSome :: Rec (Const (Some f)) xs -> Some (PairOf (SameLength xs) (Rec f))
recSequenceSome RNil = SomePair SameLength RNil
recSequenceSome (x :& xs) = case (x, recSequenceSome xs) of
  (Const (Some y), SomePair SameLength ys) -> SomePair SameLength (y :& ys)
  _ -> error "impossible"


-- This is way too general for its own good but it was fun to write.
translateOpApp
  :: (Length xs ~ n)
  => Op n ok
  -> Rec (Const (F.Expression ann)) xs -> MonadTranslate SomeExpr
translateOpApp operator argAsts = do
  someArgs <- rtraverse (fmap Const . translateExpression . getConst) argAsts

  case recSequenceSome someArgs of
    SomePair SameLength argsTranslated -> do
      let argsD = rmap (\(PairOf d _) -> d) argsTranslated
          argsExpr = rmap (\(PairOf _ e) -> e) argsTranslated

      MatchOpR opResult resultD <- case matchOpR operator argsD of
        Just x -> return x
        Nothing -> throwError $ ErrInvalidOpApplication (Some argsD)

      return $ SomePair resultD $ EOp $ FortranOp operator opResult argsExpr
    _ -> error "impossible"


translateOp2App :: F.Expression ann -> F.Expression ann -> F.BinaryOp -> MonadTranslate SomeExpr
translateOp2App e1 e2 bop = do
  Some operator <- case translateOp2 bop of
    Just x  -> return x
    Nothing -> throwError $ ErrUnsupportedItem "binary operator"
  translateOpApp operator (Const e1 :& Const e2 :& RNil)


translateOp1App :: F.Expression ann -> F.UnaryOp -> MonadTranslate SomeExpr
translateOp1App e uop = do
  Some operator <- case translateOp1 uop of
    Just x -> return x
    Nothing -> throwError $ ErrUnsupportedItem "unary operator"
  translateOpApp operator (Const e :& RNil)

--------------------------------------------------------------------------------
--  Readers for things that are strings in the AST
--------------------------------------------------------------------------------

readLitInteger :: String -> Maybe Integer
readLitInteger = readMaybe

readLitReal :: String -> Maybe Double
readLitReal = readMaybe

readLitBool :: String -> Maybe Bool
readLitBool l = case map toLower l of
  ".true."  -> Just True
  ".false." -> Just False
  _         -> Nothing

--------------------------------------------------------------------------------
--  Dealing with all the confusing ways of specifying Fortran types
--------------------------------------------------------------------------------

data TypeInfo ann =
  TypeInfo
  { tiSrcSpan :: F.SrcSpan
  , tiBaseType :: F.BaseType
  , tiSelectorLength :: Maybe (F.Expression ann)
  , tiSelectorKind :: Maybe (F.Expression ann)
  , tiDeclLength :: Maybe (F.Expression ann)
  , tiDimensionDecls :: Maybe (F.AList F.DimensionDeclarator ann)
  , tiAttributes :: Maybe (F.AList F.Attribute ann)
  }
  deriving (Show)

instance F.Spanned (TypeInfo ann) where
  getSpan = tiSrcSpan
  setSpan sp ti = ti { tiSrcSpan = sp }

typeInfo :: F.TypeSpec ann -> TypeInfo ann
typeInfo ts@(F.TypeSpec _ _ bt mselector) =
  let selectorLength (F.Selector _ _ l _) = l
      selectorKind (F.Selector _ _ _ k) = k
  in TypeInfo
     { tiSrcSpan = F.getSpan ts
     , tiBaseType = bt
     , tiSelectorLength = mselector >>= selectorLength
     , tiSelectorKind = mselector >>= selectorKind
     , tiDeclLength = Nothing
     , tiDimensionDecls = Nothing
     , tiAttributes = Nothing
     }

tiWithDimensionDecls
  :: Maybe (F.AList F.DimensionDeclarator ann)
     -> TypeInfo ann -> TypeInfo ann
tiWithDimensionDecls decls ti = ti { tiDimensionDecls = decls }

tiWithAttributes
  :: Maybe (F.AList F.Attribute ann)
     -> TypeInfo ann -> TypeInfo ann
tiWithAttributes attrs ti = ti { tiAttributes = attrs }

tiWithDeclLength
  :: Maybe (F.Expression ann) -> TypeInfo ann -> TypeInfo ann
tiWithDeclLength len ti = ti { tiDeclLength = len }

--------------------------------------------------------------------------------
--  Dynamically typed expressions
--------------------------------------------------------------------------------

translateAtType
  :: Text
  -> D b
  -> (a -> MonadTranslate SomeExpr)
  -> a -> MonadTranslate (FortranExpr b)
translateAtType langPart db translate x =
  do SomePair da someY <- translate x
     case dcast da db someY of
       Just y  -> return y
       Nothing -> throwError $ ErrUnexpectedType langPart (Some da) (Some db)
