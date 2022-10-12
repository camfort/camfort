{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE CPP                   #-}

{-# OPTIONS_GHC -Wall #-}

{-|
Actual Fortran language operators. For expressions over normal Fortran values
that are actually representable in Fortran.

+, -, *, /, read array, etc...
-}
module Language.Fortran.Model.Op.Core
  (
    CoreOp(..)
  , Op(..)
  , OpKind(..)
  , OpSpec(..)
  ) where

import           Data.Functor.Compose

#if MIN_VERSION_singletons(3,0,0)
import           Data.List.Singletons
import           GHC.TypeLits.Singletons
#else
import           Data.Singletons.Prelude.List
import           Data.Singletons.TypeLits
#endif

import           Data.Vinyl
import           Data.Vinyl.Curry

import           Language.Expression
import           Language.Expression.Pretty

import           Language.Fortran.Model.Repr
import           Language.Fortran.Model.Op.Core.Core
import           Language.Fortran.Model.Op.Core.Eval
import           Language.Fortran.Model.Singletons
import           Language.Fortran.Model.Types


data CoreOp t a where
  CoreOp
    :: Op (Length args) ok
    -> OpSpec ok args result
    -> Rec t args
    -> CoreOp t result

instance HFunctor CoreOp where
instance HTraversable CoreOp where
  htraverse f (CoreOp op opr args) = CoreOp op opr <$> rtraverse f args

instance (MonadEvalFortran r m) => HFoldableAt (Compose m CoreRepr) CoreOp where
  hfoldMap = implHfoldMapCompose $ \(CoreOp op opr args) -> evalCoreOp op opr args

instance (MonadEvalFortran r m) => HFoldableAt (Compose m HighRepr) CoreOp where
  hfoldMap = implHfoldMapCompose $ fmap HRCore . hfoldA .
    hmap (\case
               HRCore x -> x
               HRHigh _ -> error "impossible")

instance Pretty2 CoreOp where
  prettys2Prec p (CoreOp op opr args) = prettysPrecOp p opr op args

showsPrim :: Prim p k a -> a -> ShowS
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

prettysPrecOp
  :: Pretty1 t
  => Int
  -> OpSpec ok args result
  -> Op (Length args) ok
  -> Rec t args -> ShowS
prettysPrecOp p = \case
  OSLit px x -> \case
    OpLit -> runcurry $ showsPrim px x
  OSNum1 _ _ _ -> \case
    OpNeg -> runcurry $ prettys1PrecUnop 8 "-" p
    OpPos -> runcurry $ prettys1PrecUnop 8 "+" p
  OSNum2 _ _ _ _ _ -> \case
    OpAdd -> runcurry $ prettys1PrecBinop 5 " + " p
    OpSub -> runcurry $ prettys1PrecBinop 5 " - " p
    OpMul -> runcurry $ prettys1PrecBinop 6 " * " p
    OpDiv -> runcurry $ prettys1PrecBinop 6 " / " p
  OSLogical1 _ _ -> \case
    OpNot -> runcurry $ prettys1PrecUnop 8 "!" p
  OSLogical2 _ _ _ -> \case
    OpAnd      -> runcurry $ prettys1PrecBinop 3 " && " p
    OpOr       -> runcurry $ prettys1PrecBinop 2 " || " p
    OpEquiv    -> runcurry $ prettys1PrecBinop 1 " <=> " p
    OpNotEquiv -> runcurry $ prettys1PrecBinop 1 " </=> " p
  OSEq _ _ _ _ -> \case
    OpEq -> runcurry $ prettys1PrecBinop 4 " = " p
    OpNE -> runcurry $ prettys1PrecBinop 4 " /= " p
  OSRel _ _ _ _ -> \case
    OpLT -> runcurry $ prettys1PrecBinop 4 " < " p
    OpLE -> runcurry $ prettys1PrecBinop 4 " <= " p
    OpGT -> runcurry $ prettys1PrecBinop 4 " > " p
    OpGE -> runcurry $ prettys1PrecBinop 4 " >= " p
  OSLookup _ -> \case
    OpLookup ->
      runcurry $ \arr i ->
      showParen (p > 9) $ prettys1Prec 10 arr .
                          showString "[" . prettys1Prec 0 i .
                          showString "]"
  OSDeref _ fname -> \case
    OpDeref -> runcurry $ \r ->
      showParen (p > 9) $ prettys1Prec 10 r .
      showString "%" .
      showString (withKnownSymbol fname (symbolVal fname))

-- TODO: HEq instance

-- instance HEq CoreOp where
--   liftHEq he le (CoreOp op1 opr1 args1) (CoreOp op2 opr2 args2) =
--     eqOp op1 op2 &&
--     eqOpR opr1 opr2 &&
--     liftEqRec (he _) args1 args2
