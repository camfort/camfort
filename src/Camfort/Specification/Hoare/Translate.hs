{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wall #-}

{-|

Translation from annotation syntax defined in
"Camfort.Specification.Hoare.Syntax" to strongly-typed meta-expressions defined
in "Language.Fortran.Model.Op.Meta".

-}
module Camfort.Specification.Hoare.Translate
  (
    -- * Meta Expression Types
    MetaExpr
  , MetaFormula
  , AllOps

    -- * Translation
  , translateBoolExpression
  , translateFormula

    -- * Combinators
  , intoMetaExpr
  ) where

import           Prelude                            hiding (span)

import           Control.Lens
import           Control.Monad.Except               (MonadError (..))
import           Control.Monad.Fail

import qualified Language.Fortran.Analysis          as F
import qualified Language.Fortran.AST               as F

import           Language.Expression
import           Language.Expression.Choice
import           Language.Expression.Prop

import           Camfort.Helpers.TypeLevel
import           Language.Fortran.Model
import           Language.Fortran.Model.Singletons
import           Language.Fortran.Model.Translate
import           Language.Fortran.Model.Types.Match
import           Language.Fortran.Model.Vars

import           Camfort.Specification.Hoare.Syntax

--------------------------------------------------------------------------------
--  Lifting Logical Values
--------------------------------------------------------------------------------

type AllOps = '[HighOp, MetaOp, CoreOp]
type MetaExpr = HFree' AllOps
type MetaFormula = Prop (MetaExpr FortranVar)

--------------------------------------------------------------------------------
--  Translate
--------------------------------------------------------------------------------

-- | Translate an untyped logical formula into a strongly typed 'MetaFormula'.
translateFormula :: (Monad m, MonadFail m) => PrimFormula (F.Analysis ann) -> TranslateT m (MetaFormula Bool)
translateFormula = \case
  PFExpr e -> do
    e' <- translateBoolExpression e
    return $ expr $ e'

  PFLogical x -> foldPrimLogic <$> traverse translateFormula x


-- | Translate a boolean-valued untyped Fortran expression into a strongly typed 'MetaExpr'.
translateBoolExpression
  :: (Monad m, MonadFail m)
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


foldPrimLogic :: PrimLogic (MetaFormula Bool) -> MetaFormula Bool
foldPrimLogic = \case
  PLAnd x y -> x *&& y
  PLOr x y -> x *|| y
  PLImpl x y -> x *-> y
  PLEquiv x y -> x *<-> y
  PLNot x -> pnot x
  PLLit x -> plit x


--------------------------------------------------------------------------------
--  Util
--------------------------------------------------------------------------------

-- | Convert an expression over 'HighOp', 'MetaOp' or 'CoreOp' into a 'MetaExpr'.
intoMetaExpr :: (ChooseOp op AllOps, HTraversable op) => HFree op v a -> MetaExpr v a
intoMetaExpr = HFree' . hduomapFirst' (review chooseOp)

liftFortranExpr :: (LiftD a b) => FortranExpr a -> MetaExpr FortranVar b
liftFortranExpr e =
  let e' = HWrap (HopLift (LiftDOp (HPure e)))
  in squashExpression e'
