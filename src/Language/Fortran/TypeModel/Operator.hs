{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wall #-}

module Language.Fortran.TypeModel.Operator (FortranOp(..)) where

import           Data.Singletons.Prelude.List
import           Data.Singletons.TypeLits

import           Data.Vinyl
import           Data.Vinyl.Curry

import           Language.Expression
import           Language.Expression.Pretty

import           Language.Fortran.TypeModel.Operator.Core
import           Language.Fortran.TypeModel.Operator.Eval
import           Language.Fortran.TypeModel.Types


data FortranOp t a where
  FortranOp :: Op (Length args) ok -> OpResult ok args result -> Rec t args -> FortranOp t result

instance Operator FortranOp where
  htraverseOp f (FortranOp op opr args) = FortranOp op opr <$> rtraverse f args

instance (Applicative f) => EvalOp f SymRepr FortranOp where
  evalOp f (FortranOp op opr args) = evalFortranOp op opr <$> rtraverse f args

instance Pretty2 FortranOp where
  prettys2Prec p (FortranOp op opr args) = prettysPrecOp p opr op args

showsPrim :: Prim p k (PrimS a) -> a -> ShowS
showsPrim = \case
  PInt8   -> shows
  PInt16  -> shows
  PInt32  -> shows
  PInt64  -> shows
  PBool8  -> shows
  PBool16 -> shows
  PBool32 -> shows
  PBool64 -> shows
  PFloat  -> shows
  PDouble -> shows
  PChar   -> shows

prettysPrecOp :: Pretty1 t => Int -> OpResult ok args result -> Op (Length args) ok -> Rec t args -> ShowS
prettysPrecOp p = \case
  ORLit px x -> \case
    OpLit -> runcurry $ showsPrim px x
  ORNum1 _ _ _ -> \case
    OpNeg -> runcurry $ prettys1PrecUnop 8 "-" p
    OpPos -> runcurry $ prettys1PrecUnop 8 "+" p
  ORNum2 _ _ _ _ _ -> \case
    OpAdd -> runcurry $ prettys1PrecBinop 5 " + " p
    OpSub -> runcurry $ prettys1PrecBinop 5 " - " p
    OpMul -> runcurry $ prettys1PrecBinop 6 " * " p
    OpDiv -> runcurry $ prettys1PrecBinop 6 " / " p
  ORLogical1 _ _ -> \case
    OpNot -> runcurry $ prettys1PrecUnop 8 "!" p
  ORLogical2 _ _ _ -> \case
    OpAnd      -> runcurry $ prettys1PrecBinop 3 " && " p
    OpOr       -> runcurry $ prettys1PrecBinop 2 " || " p
    OpEquiv    -> runcurry $ prettys1PrecBinop 1 " <=> " p
    OpNotEquiv -> runcurry $ prettys1PrecBinop 1 " </=> " p
  OREq _ _ _ _ -> \case
    OpEq -> runcurry $ prettys1PrecBinop 4 " = " p
    OpNE -> runcurry $ prettys1PrecBinop 4 " /= " p
  ORRel _ _ _ _ -> \case
    OpLT -> runcurry $ prettys1PrecBinop 4 " < " p
    OpLE -> runcurry $ prettys1PrecBinop 4 " <= " p
    OpGT -> runcurry $ prettys1PrecBinop 4 " > " p
    OpGE -> runcurry $ prettys1PrecBinop 4 " >= " p
  ORLookup _ -> \case
    OpLookup ->
      runcurry $ \arr i ->
      showParen (p > 9) $ prettys1Prec 10 arr .
                          showString "[" . prettys1Prec 0 i .
                          showString "]"
  ORDeref _ fname -> \case
    OpDeref -> runcurry $ \r ->
      showParen (p > 9) $ prettys1Prec 10 r .
      showString "%" .
      showString (withKnownSymbol fname (symbolVal fname))

-- TODO: HEq instance

-- instance HEq FortranOp where
--   liftHEq he le (FortranOp op1 opr1 args1) (FortranOp op2 opr2 args2) =
--     eqOp op1 op2 &&
--     eqOpR opr1 opr2 &&
--     liftEqRec (he _) args1 args2
