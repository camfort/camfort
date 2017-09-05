{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -Wall #-}

{-|
Operators for expressions over lifted values

- Lifting Fortran types to higher-level representations
- Folds over arrays (sum, product)
- High-level mathematical functions (factorial...)
-}

module Language.Fortran.Model.Op.High where


import           Control.Monad.Reader.Class       (MonadReader, asks)

import           Language.Expression
import           Language.Expression.Pretty

import           Language.Fortran.Model.Repr
import           Language.Fortran.Model.Repr.Prim

--------------------------------------------------------------------------------
--  High-level Operations
--------------------------------------------------------------------------------

data HighOp t a where
  HopLift :: LiftDOp t a -> HighOp t a


instance Operator HighOp where
  htraverseOp f = \case
    HopLift x -> HopLift <$> htraverseOp f x

instance (MonadReader r m, HasPrimReprHandlers r) => EvalOp m HighRepr HighOp where
  evalOp = \case
    HopLift x -> evalOp x

--------------------------------------------------------------------------------
--  Lifting Fortran values
--------------------------------------------------------------------------------

data LiftDOp t a where
  LiftDOp :: LiftD b a => t b -> LiftDOp t a

instance Operator LiftDOp where
  htraverseOp f = \case
    LiftDOp x -> LiftDOp <$> f x

instance (MonadReader r m, HasPrimReprHandlers r
         ) => EvalOp m HighRepr LiftDOp where
  evalOp = \case
    LiftDOp x -> do
      env <- asks primReprHandlers
      pure $ liftDRepr env x

instance Pretty2 LiftDOp where
  prettys2Prec p = \case
    -- TODO: Consider adding printed evidence of the lifting
    LiftDOp x -> prettys1Prec p x
