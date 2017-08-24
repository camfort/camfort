{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wall      #-}

module Language.Fortran.TypeModel.Operator.Eval where

import           Data.SBV.Dynamic                         hiding (KReal)

import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Singletons.TypeLits

import           Data.Vinyl
import           Data.Vinyl.Curry

import           Language.Fortran.TypeModel.Operator.Core
import           Language.Fortran.TypeModel.Singletons
import           Language.Fortran.TypeModel.Types

evalFortranOp :: Op (Length args) ok -> OpResult ok args result -> Rec SymRepr args -> SymRepr result
evalFortranOp op opr = case opr of
  ORLit px x -> \_ -> primFromVal px (primLit px x)

  ORNum1 _ p1 p2 ->
    primUnop p1 p2 (numUnop op)
  ORNum2 nk1 nk2 p1 p2 p3 ->
    primBinop p1 p2 p3 (numBinop (nkBothInts nk1 nk2) op)

  ORLogical1 p1 p2 -> primUnop p1 p2 (logicalUnop op)
  ORLogical2 p1 p2 p3 -> primBinop p1 p2 p3 (logicalBinop op)

  OREq cmp p1 p2 p3 -> primBinop p1 p2 p3 (eqBinop cmp op)
  ORRel cmp p1 p2 p3 -> primBinop p1 p2 p3 (relBinop cmp op)

  ORLookup (DArray (Index indexPrim) elPrim) ->
    runcurry $ \xs index ->
      let xsArr = toArr xs
          indexVal = primToVal indexPrim index
      in primFromVal elPrim (readSArr xsArr indexVal)

  ORDeref _ fname -> runcurry (\r -> derefRec (toRec r) fname)

--------------------------------------------------------------------------------
--  General
--------------------------------------------------------------------------------

primToVal :: Prim p k a -> SymRepr a -> SVal
primToVal p = primS p $ const $ \case
  SRPrim (DPrim _) v -> v

primFromVal :: Prim p k a -> SVal -> SymRepr a
primFromVal p v = primS p $ \p' -> SRPrim (DPrim p') v

toArr :: SymRepr (Array i v) -> SArr
toArr (SRArray _ x) = x

fromArr :: Index i -> Prim p k a -> SArr -> SymRepr (Array i a)
fromArr index elPrim = SRArray (DArray index elPrim)

toRec :: SymRepr (Record rname fields) -> Rec FieldRepr fields
toRec (SRData _ x) = x

primUnop
  :: Prim p1 k1 a -> Prim p2 k2 b
  -> (SVal -> SVal)
  -> Rec SymRepr '[a] -> SymRepr b
primUnop p1 p2 f = primFromVal p2 . runcurry (f . primToVal p1)

primBinop
  :: Prim p1 k1 a -> Prim p2 k2 b -> Prim p3 k3 c
  -> (SVal -> SVal -> SVal)
  -> Rec SymRepr '[a, b] -> SymRepr c
primBinop p1 p2 p3 f = primFromVal p3 . runcurry (\x y -> f (primToVal p1 x) (primToVal p2 y))

--------------------------------------------------------------------------------
--  Literals
--------------------------------------------------------------------------------

primLit :: Prim p k (PrimS a) -> a -> SVal
primLit = \case
  PInt8  -> svIntegral
  PInt16 -> svIntegral
  PInt32 -> svIntegral
  PInt64 -> svIntegral

  PBool8  -> svBool' . getBool8
  PBool16 -> svBool' . getBool16
  PBool32 -> svBool' . getBool32
  PBool64 -> svBool' . getBool64

  PFloat -> svFloat
  PDouble -> svDouble

  PChar -> svIntegral . getChar8

  where
    svIntegral x = svInteger (kindOf x) (fromIntegral x)

    -- Representing 1-bit truth values compactly when possible makes everything
    -- much faster.
    svBool' x
      | x == 0 = svFalse
      | x == 1 = svTrue
      | otherwise = svIntegral x

--------------------------------------------------------------------------------
--  Numeric
--------------------------------------------------------------------------------

-- TODO: Worry about what happens when LHS and RHS have different types

nkBothInts :: NumericKind k1 -> NumericKind k2 -> Bool
nkBothInts NKInt NKInt = True
nkBothInts _ _         = False

numUnop :: Op 1 'OKNum -> SVal -> SVal
numUnop = \case
  OpNeg -> svUNeg
  OpPos -> id

numBinop :: Bool -> Op 2 'OKNum -> SVal -> SVal -> SVal
numBinop isInt = \case
  OpAdd -> svPlus
  OpSub -> svMinus
  OpMul -> svTimes
  OpDiv -> if isInt then svQuot else svDivide

--------------------------------------------------------------------------------
--  Logical
--------------------------------------------------------------------------------

-- TODO: Always return single bits, special-case when inputs are not single bits.

logicalUnop :: Op 1 'OKLogical -> SVal -> SVal
logicalUnop = \case
  OpNot -> svNot

logicalBinop :: Op 2 'OKLogical -> SVal -> SVal -> SVal
logicalBinop = \case
  OpAnd -> svAnd
  OpOr -> svOr
  OpEquiv -> svEquiv
  OpNotEquiv -> svNotEquiv
  where
    svEquiv x y = (x `svAnd` y) `svOr` (svNot x `svAnd` svNot y)
    svNotEquiv x y = svNot (x `svEquiv` y)

--------------------------------------------------------------------------------
--  Equality
--------------------------------------------------------------------------------

-- TODO: Worry about what happens when LHS and RHS have different types

eqBinop :: ComparableKinds k1 k2 -> Op 2 'OKEq -> SVal -> SVal -> SVal
eqBinop _ = \case
  OpEq -> svEqual
  OpNE -> svNotEqual

--------------------------------------------------------------------------------
--  Relational
--------------------------------------------------------------------------------

-- TODO: Worry about what happens when LHS and RHS have different types

relBinop :: ComparableKinds k1 k2 -> Op 2 'OKRel -> SVal -> SVal -> SVal
relBinop _ = \case
  OpLT -> svLessThan
  OpLE -> svLessEq
  OpGT -> svGreaterThan
  OpGE -> svGreaterEq

--------------------------------------------------------------------------------
--  Deref
--------------------------------------------------------------------------------

derefRec :: forall fname a fields i. RElem '(fname, a) fields i => Rec FieldRepr fields -> SSymbol fname -> SymRepr a
derefRec r _ =
  case rget (Proxy :: Proxy '(fname, a)) r of
    FR _ x -> x

--------------------------------------------------------------------------------
--  Equality of operators
--------------------------------------------------------------------------------

-- eqPrim :: Prim p1 k1 a -> Prim p2 k2 b -> Bool
-- eqPrim p1 p2 = case (p1, p2) of
--   (PInt8, PInt8) -> True
--   (PInt16, PInt16) -> True
--   (PInt32, PInt32) -> True
--   (PInt64, PInt64) -> True
--   (PBool8, PBool8) -> True
--   (PBool16, PBool16) -> True
--   (PBool32, PBool32) -> True
--   (PBool64, PBool64) -> True
--   (PFloat, PFloat) -> True
--   (PDouble, PDouble) -> True
--   (PChar, PChar) -> True
--   _ -> False

-- eqPrimS :: Prim p1 k1 a -> Prim p2 k2 b -> a -> b -> Bool
-- eqPrimS p1 p2 = case (p1, p2) of
--   (PInt8, PInt8) -> (==)
--   (PInt16, PInt16) -> (==)
--   (PInt32, PInt32) -> (==)
--   (PInt64, PInt64) -> (==)
--   (PBool8, PBool8) -> (==)
--   (PBool16, PBool16) -> (==)
--   (PBool32, PBool32) -> (==)
--   (PBool64, PBool64) -> (==)
--   (PFloat, PFloat) -> (==)
--   (PDouble, PDouble) -> (==)
--   (PChar, PChar) -> (==)
--   _ -> \_ _ -> False

-- eqSymbol :: SSymbol n1 -> SSymbol n2 -> Bool
-- eqSymbol n1 n2 = withKnownSymbol n1 $ withKnownSymbol n2 $ symbolVal n1 == symbolVal n2

-- eqD :: D a -> D b -> Bool
-- eqD d1 d2 = case (d1, d2) of
--   (DPrim p1, DPrim p2) -> eqPrim p1 p2
--   (DArray (Index i1) p1, DArray (Index i2) p2) -> eqPrim i1 i2 && eqPrim p1 p2
--   (DData n1 r1, DData n2 r2) -> eqSymbol n1 n2 && liftEqRec eqField r1 r2
--   _ -> False

-- eqField :: RField a -> RField b -> Bool
-- eqField (RField n1 d1) (RField n2 d2) = eqSymbol n1 n2 && eqD d1 d2

-- eqOp :: Op n1 ok1 -> Op n2 ok2 -> Bool
-- eqOp o1 o2 = case (o1, o2) of
--   (OpLit, OpLit) -> True
--   (OpNeg, OpNeg) -> True
--   (OpPos, OpPos) -> True
--   (OpAdd, OpAdd) -> True
--   (OpSub, OpSub) -> True
--   (OpMul, OpMul) -> True
--   (OpDiv, OpDiv) -> True
--   (OpEq, OpEq) -> True
--   (OpNE, OpNE) -> True
--   (OpLT, OpLT) -> True
--   (OpLE, OpLE) -> True
--   (OpGT, OpGT) -> True
--   (OpGE, OpGE) -> True
--   (OpNot, OpNot) -> True
--   (OpAnd, OpAnd) -> True
--   (OpOr, OpOr) -> True
--   (OpEquiv, OpEquiv) -> True
--   (OpNotEquiv, OpNotEquiv) -> True
--   (OpLookup, OpLookup) -> True
--   (OpDeref, OpDeref) -> True
--   (_, _) -> False

-- eqOpR :: OpResult ok1 args1 r1 -> OpResult ok2 args2 r2 -> Bool
-- eqOpR o1 o2 = case (o1, o2) of
--   (ORLit p1 x1, ORLit p2 x2) -> eqPrimS p1 p2 (PrimS x1) (PrimS x2)
--   (ORNum1 _ px1 px2, ORNum1 _ py1 py2) -> eqPrim px1 py1 && eqPrim px2 py2
--   (ORNum2 _ _ px1 px2 px3, ORNum2 _ _ py1 py2 py3) -> eqPrim px1 py1 && eqPrim px2 py2 && eqPrim px3 py3
--   (ORLogical1 px1 px2, ORLogical1 py1 py2) -> eqPrim px1 py1 && eqPrim px2 py2
--   (ORLogical2 px1 px2 px3, ORLogical2 py1 py2 py3) -> eqPrim px1 py1 && eqPrim px2 py2 && eqPrim px3 py3
--   (OREq _ px1 px2 px3, OREq _ py1 py2 py3) -> eqPrim px1 py1 && eqPrim px2 py2 && eqPrim px3 py3
--   (ORRel _ px1 px2 px3, ORRel _ py1 py2 py3) -> eqPrim px1 py1 && eqPrim px2 py2 && eqPrim px3 py3
--   (ORLookup d1, ORLookup d2) -> eqD d1 d2
--   (ORDeref d1 n1, ORDeref d2 n2) -> eqD d1 d2 && eqSymbol n1 n2
--   _ -> False

-- eqOpRArgs
--   :: (forall x y. (x -> y -> Bool) -> f x -> g y -> Bool)
--   -> OpResult ok1 args1 r1
--   -> OpResult ok2 args2 r2
--   -> Rec f args1
--   -> Rec g args2
--   -> Bool
-- eqOpRArgs le opr1 opr2 =
--   case (opr1, opr2) of
--     (ORLit _ _, ORLit _ _) -> \_ _ -> True
--     (ORNum1 _ px _, ORNum1 _ py _) -> runcurry $ runcurry . le (eqPrimS px py)
--     (ORNum2 _ _ px1 px2 _, ORNum2 _ _ py1 py2 _) ->
--       runcurry $ \x1 x2 -> runcurry $ \y1 y2 ->
--       le (eqPrimS px1 py1) x1 y1 && le (eqPrimS px2 py2) x2 y2
--     (ORLogical1 px _, ORLogical1 py _) -> runcurry $ runcurry . le (eqPrimS px py)
--     (ORLogical2 px1 px2 _, ORLogical2 py1 py2 _) ->
--       runcurry $ \x1 x2 -> runcurry $ \y1 y2 ->
--       le (eqPrimS px1 py1) x1 y1 && le (eqPrimS px2 py2) x2 y2
--     (OREq _ px1 px2 _, OREq _ py1 py2 _) ->
--       runcurry $ \x1 x2 -> runcurry $ \y1 y2 ->
--       le (eqPrimS px1 py1) x1 y1 && le (eqPrimS px2 py2) x2 y2
--     (ORRel _ px1 px2 _, ORRel _ py1 py2 _) ->
--       runcurry $ \x1 x2 -> runcurry $ \y1 y2 ->
--       le (eqPrimS px1 py1) x1 y1 && le (eqPrimS px2 py2) x2 y2
--     (ORLookup (DArray (Index pi1) _), ORLookup (DArray (Index pi2) _)) ->
--       runcurry $ \arr1 i1 -> runcurry $ \arr2 i2 ->
--       le _ arr1 arr2 && le (eqPrimS pi1 pi2) i1 i2
--       -- le ()

-- liftEqRec
--   :: (forall a b. f a -> g b -> Bool)
--   -> Rec f xs -> Rec g ys
--   -> Bool
-- liftEqRec f r1 r2 = case (r1, r2) of
--   (RNil, RNil) -> True
--   (h1 :& t1, h2 :& t2) -> f h1 h2 && liftEqRec f t1 t2
--   _ -> False
