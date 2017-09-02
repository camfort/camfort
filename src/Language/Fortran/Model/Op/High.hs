{-# LANGUAGE DataKinds              #-}
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

{-# OPTIONS_GHC -Wall      #-}

{-|
Operators for expressions over lifted values

- Lifting Fortran types to higher-level representations
- Folds over arrays (sum, product)
- High-level mathematical functions (factorial...)
-}

module Language.Fortran.Model.Op.High where


import           Control.Monad.Reader.Class      (MonadReader (ask))

import           Language.Expression
import           Language.Expression.Pretty

import           Language.Fortran.Model.Repr.Prim
import           Language.Fortran.Model.Repr

--------------------------------------------------------------------------------
--  Lifting Fortran values
--------------------------------------------------------------------------------

data LiftDOp t a where
  LiftDOp :: LiftD b a => t b -> LiftDOp t a

instance Operator LiftDOp where
  htraverseOp f = \case
    LiftDOp x -> LiftDOp <$> f x

instance (MonadReader r m, HasPrimReprHandler r
         ) => EvalOp m HighRepr LiftDOp where
  evalOp f = \case
    LiftDOp x -> do
      env <- ask
      let env' = PrimReprHandlers (getSymRepr env)
      liftDRepr env' <$> f x

instance Pretty2 LiftDOp where
  prettys2Prec p = \case
    -- TODO: Consider adding printed evidence of the lifting
    LiftDOp x -> prettys1Prec p x
