{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall #-}

{-|

Translation of meta-expressions.

-}
module Camfort.Specification.Hoare.Translate
  (
    MetaExpr
  , MetaFormula
  , FLiftLogical

  , translateBoolExpression
  , translateFormula
  , fortranToMetaExpr
  ) where

import           Prelude                               hiding (span)

import           Control.Monad.Except                  (MonadError (..))

import           Data.SBV.Dynamic                      (svFalse, svGreaterThan)
import           Data.SBV.Internals                    (SBV (SBV))

import qualified Language.Fortran.AST                  as F

import           Language.Expression
import           Language.Expression.Pretty
import           Language.Expression.Prop

import           Camfort.Helpers.TypeLevel
import           Language.Fortran.TypeModel
import           Language.Fortran.TypeModel.Match
import           Language.Fortran.TypeModel.Singletons
import           Language.Fortran.TypeModel.Translate
import           Language.Fortran.TypeModel.Vars

import           Camfort.Specification.Hoare.Syntax

--------------------------------------------------------------------------------
--  Lifting Logical Values
--------------------------------------------------------------------------------

type MetaExpr = Expr' [FLiftLogical, FortranOp]
type MetaFormula = Prop (MetaExpr FortranVar)

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
--  Translate
--------------------------------------------------------------------------------

translateFormula :: (Monad m) => PrimFormula ann -> TranslateT m (MetaFormula Bool)
translateFormula = \case
  PFExpr e -> do
    e' <- translateBoolExpression e
    return $ expr $ e'

  PFLogical x -> translateLogical <$> traverse translateFormula x


translateBoolExpression
  :: (Monad m) => F.Expression ann
  -> TranslateT m (MetaExpr FortranVar Bool)
translateBoolExpression e = do
  SomePair d1 e' <- translateExpression e

  resUnsquashed :: Expr FLiftLogical FortranExpr Bool <- case matchPrimD d1 of
    Just (MatchPrimD (MatchPrim _ SBTLogical) prim1) -> return $ EOp $
      case prim1 of
        PBool8  -> FLL8 (EVar e')
        PBool16 -> FLL16 (EVar e')
        PBool32 -> FLL32 (EVar e')
        PBool64 -> FLL64 (EVar e')
    _ -> throwError $ ErrUnexpectedType "formula" (Some (DPrim PBool8)) (Some d1)

  return (squashExpression resUnsquashed)


translateLogical :: PrimLogic (MetaFormula Bool) -> MetaFormula Bool
translateLogical = \case
  PLAnd x y -> x *&& y
  PLOr x y -> x *|| y
  PLImpl x y -> x *-> y
  PLEquiv x y -> x *<-> y
  PLNot x -> pnot x
  PLLit x -> plit x


--------------------------------------------------------------------------------
--  Util
--------------------------------------------------------------------------------

fortranToMetaExpr :: FortranExpr a -> MetaExpr FortranVar a
fortranToMetaExpr (e :: FortranExpr a) =
  let e' :: Expr FLiftLogical (Expr FortranOp FortranVar) a
      e' = EVar e
  in squashExpression e'
