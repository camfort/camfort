{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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

  , translateBoolExpression
  , translateFormula
  , intoMetaExpr
  ) where

import           Prelude                            hiding (span)

import           Control.Lens
import           Control.Monad.Except               (MonadError (..))

import qualified Language.Fortran.Analysis          as F
import qualified Language.Fortran.AST               as F

import           Language.Expression
import           Language.Expression.Prop

import           Camfort.Helpers.TypeLevel
import           Language.Fortran.Model
import           Language.Fortran.Model.Repr
import           Language.Fortran.Model.Singletons
import           Language.Fortran.Model.Translate
import           Language.Fortran.Model.Types.Match
import           Language.Fortran.Model.Vars

import           Camfort.Specification.Hoare.Syntax

--------------------------------------------------------------------------------
--  Lifting Logical Values
--------------------------------------------------------------------------------

type AllOps = '[HighOp, MetaOp, CoreOp]
type MetaExpr = Expr' AllOps
type MetaFormula = Prop (MetaExpr FortranVar)

--------------------------------------------------------------------------------
--  Translate
--------------------------------------------------------------------------------

translateFormula :: (Monad m) => PrimFormula (F.Analysis ann) -> TranslateT m (MetaFormula Bool)
translateFormula = \case
  PFExpr e -> do
    e' <- translateBoolExpression e
    return $ expr $ e'

  PFLogical x -> translateLogical <$> traverse translateFormula x


translateBoolExpression
  :: (Monad m)
  => F.Expression (F.Analysis ann)
  -> TranslateT m (MetaExpr FortranVar Bool)
translateBoolExpression e = do
  SomePair d1 e' <- translateExpression e

  case matchPrimD d1 of
    Just (MatchPrimD (MatchPrim _ SBTLogical) prim1) -> return $
      case prim1 of
        PBool8  -> liftFortranExpr e'
        PBool16 -> liftFortranExpr e'
        PBool32 -> liftFortranExpr e'
        PBool64 -> liftFortranExpr e'
    _ -> throwError $ ErrUnexpectedType "formula" (Some (DPrim PBool8)) (Some d1)


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

liftFortranExpr :: (LiftD a b) => FortranExpr a -> MetaExpr FortranVar b
liftFortranExpr e =
  let e' = EOp (HopLift (LiftDOp (EVar e)))
  in squashExpression e'


intoMetaExpr :: (ChooseOp op AllOps, Operator op) => Expr op v a -> MetaExpr v a
intoMetaExpr = Expr' . mapOperators (review chooseOp)
