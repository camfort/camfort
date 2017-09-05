{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}

{-|
For expressions over normal Fortran values that are not representable in Fortran.

- Immutable array update
- Immutable data update
-}
module Language.Fortran.Model.Op.Meta where

import Language.Fortran.Model.Repr

import Language.Expression
import Language.Expression.Pretty


data MetaOp t a where


instance Operator MetaOp where
  htraverseOp _ = \case

instance (Applicative f) => EvalOp f HighRepr MetaOp where
  evalOp = \case

instance Pretty2 MetaOp where
  prettys2Prec _ = \case
