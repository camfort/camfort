{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeApplications       #-}

{-# OPTIONS_GHC -Wall #-}

module Language.Fortran.Model.Op.Core.Eval
  ( MonadEvalFortran
  , evalCoreOp
  ) where

import           Control.Applicative                  (liftA2)

import           Data.SBV.Dynamic                     (SVal)
import qualified Data.SBV.Dynamic                     as SBV

import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Singletons.TypeLits

import           Data.Vinyl                           hiding (Field)
import           Data.Vinyl.Curry

import           Language.Fortran.Model.Op.Core.Core
import           Language.Fortran.Model.Op.Core.Match
import           Language.Fortran.Model.Op.Eval
import           Language.Fortran.Model.Repr
import           Language.Fortran.Model.Repr.Prim
import           Language.Fortran.Model.Singletons
import           Language.Fortran.Model.Types
import           Language.Fortran.Model.Types.Match

--------------------------------------------------------------------------------
--  Evaluation
--------------------------------------------------------------------------------

evalCoreOp
  :: (MonadEvalFortran r m)
  => Op (Length args) ok -> OpSpec ok args result -> Rec CoreRepr args -> m (CoreRepr result)
evalCoreOp op = \case
  OSLit px x -> \_ -> primFromVal px <$> primLit px x

  OSNum1 _ _ p2 ->
    primUnop True p2 (numUnop op)
  OSNum2 nk1 nk2 p1 p2 p3 ->
    primBinop True p1 p2 p3 (numBinop (nkBothInts nk1 nk2) op)

  OSLogical1 _ p2 -> primUnop True p2 (logicalUnop op)
  OSLogical2 p1 p2 p3 -> primBinop True p1 p2 p3 (logicalBinop op)

  OSEq cmp p1 p2 p3 -> primBinop False p1 p2 p3 (eqBinop cmp op)
  OSRel cmp p1 p2 p3 -> primBinop False p1 p2 p3 (relBinop cmp op)

  OSLookup _ -> return . runcurry lookupArr

  OSDeref _ s -> return . runcurry (derefData s)

--------------------------------------------------------------------------------
--  General
--------------------------------------------------------------------------------

primToVal :: CoreRepr (PrimS a) -> SVal
primToVal = \case
  CRPrim (DPrim _) v -> v

primFromVal :: Prim p k a -> SVal -> CoreRepr (PrimS a)
primFromVal p v = CRPrim (DPrim p) v

primUnop
  :: (MonadEvalFortran r m)
  => Bool
  -> Prim p2 k2 b -- ^ The target type
  -> (SVal -> SVal)
  -> Rec CoreRepr '[PrimS a] -> m (CoreRepr (PrimS b))
primUnop shouldCoerce p2 f = runcurry $ fmap (primFromVal p2 . f) . maybeCoerce . primToVal
  where
    maybeCoerce
      | shouldCoerce = coercePrimSVal p2
      | otherwise = return

primBinop
  :: (MonadEvalFortran r m)
  => Bool
  -- ^ True to coerce arguments to result value. False to coerce arguments to
  -- ceiling of each.
  -> Prim p1 k1 a -> Prim p2 k2 b -> Prim p3 k3 c
  -> (SVal -> SVal -> SVal)
  -> Rec CoreRepr '[PrimS a, PrimS b] -> m (CoreRepr (PrimS c))
primBinop takesResultVal p1 p2 p3 (.*.) =
  fmap (primFromVal p3) .
  runcurry (\x y -> liftA2 (.*.) (coerceArg (primToVal x)) (coerceArg (primToVal y)))

  where
    coerceToCeil = case primCeil p1 p2 of
      Just (MakePrim pCeil) -> coercePrimSVal pCeil
      _                     -> return

    coerceArg
      | takesResultVal = coercePrimSVal p3
      | otherwise = coerceToCeil

--------------------------------------------------------------------------------
--  Numeric
--------------------------------------------------------------------------------

-- TODO: Worry about what happens when LHS and RHS have different types

nkBothInts :: NumericBasicType k1 -> NumericBasicType k2 -> Bool
nkBothInts NBTInt NBTInt = True
nkBothInts _ _           = False

numUnop :: Op 1 'OKNum -> SVal -> SVal
numUnop = \case
  OpNeg -> SBV.svUNeg
  OpPos -> id

numBinop :: Bool -> Op 2 'OKNum -> SVal -> SVal -> SVal
numBinop isInt = \case
  OpAdd -> SBV.svPlus
  OpSub -> SBV.svMinus
  OpMul -> SBV.svTimes
  OpDiv -> if isInt then SBV.svQuot else SBV.svDivide

--------------------------------------------------------------------------------
--  Logical
--------------------------------------------------------------------------------

-- TODO: Always return single bits, special-case when inputs are not single bits.
-- TODO: Worry about what happens when LHS and RHS have different types

logicalUnop :: Op 1 'OKLogical -> SVal -> SVal
logicalUnop = \case
  OpNot -> SBV.svNot

logicalBinop :: Op 2 'OKLogical -> SVal -> SVal -> SVal
logicalBinop = \case
  OpAnd -> SBV.svAnd
  OpOr -> SBV.svOr
  OpEquiv -> svEquiv
  OpNotEquiv -> svNotEquiv
  where
    svEquiv x y = (x `SBV.svAnd` y) `SBV.svOr` (SBV.svNot x `SBV.svAnd` SBV.svNot y)
    svNotEquiv x y = SBV.svNot (x `svEquiv` y)

--------------------------------------------------------------------------------
--  Equality
--------------------------------------------------------------------------------

-- TODO: Worry about what happens when LHS and RHS have different types

eqBinop :: ComparableBasicTypes k1 k2 -> Op 2 'OKEq -> SVal -> SVal -> SVal
eqBinop _ = \case
  OpEq -> SBV.svEqual
  OpNE -> SBV.svNotEqual

--------------------------------------------------------------------------------
--  Relational
--------------------------------------------------------------------------------

-- TODO: Worry about what happens when LHS and RHS have different types

relBinop :: ComparableBasicTypes k1 k2 -> Op 2 'OKRel -> SVal -> SVal -> SVal
relBinop _ = \case
  OpLT -> SBV.svLessThan
  OpLE -> SBV.svLessEq
  OpGT -> SBV.svGreaterThan
  OpGE -> SBV.svGreaterEq

--------------------------------------------------------------------------------
--  Lookup
--------------------------------------------------------------------------------

lookupArrRepr
  :: CoreRepr i
  -> D (Array i v)
  -> ArrRepr i v
  -> CoreRepr v
lookupArrRepr ixRepr (DArray ixIndex@(Index _) valAV) arrRepr =
  case ixRepr of
    CRPrim _ ixVal -> case (valAV, arrRepr) of
      (ArrPrim _, ARPrim arr) ->
        CRPrim (dArrValue valAV) (SBV.readSArr arr ixVal)
      (ArrData _ dfields, ARData afields) ->
        let avD = (dArrValue valAV)
        in CRData avD (
          rzipWith
          (zipFieldsWith (lookupArrRepr ixRepr . DArray ixIndex))
          dfields
          afields)

lookupArr
  :: CoreRepr (Array i v)
  -> CoreRepr i
  -> CoreRepr v
lookupArr (CRArray arrD arrRepr) ixRepr = lookupArrRepr ixRepr arrD arrRepr

--------------------------------------------------------------------------------
--  Deref
--------------------------------------------------------------------------------

derefData
  :: forall a fname fields i rname. RElem '(fname, a) fields i
  => SSymbol fname
  -> CoreRepr (Record rname fields)
  -> CoreRepr a
derefData _ (CRData _ dataRec) =
      case rget @'(fname, a) @fields dataRec of
    Field _ x -> x

--------------------------------------------------------------------------------
--  Equality of operators
--------------------------------------------------------------------------------

-- eqOpRArgs
--   :: (forall x y. (x -> y -> Bool) -> f x -> g y -> Bool)
--   -> OpSpec ok1 args1 r1
--   -> OpSpec ok2 args2 r2
--   -> Rec f args1
--   -> Rec g args2
--   -> Bool
-- eqOpRArgs le opr1 opr2 =
--   case (opr1, opr2) of
--     (OSLit _ _, OSLit _ _) -> \_ _ -> True
--     (OSNum1 _ px _, OSNum1 _ py _) -> runcurry $ runcurry . le (eqPrimS px py)
--     (OSNum2 _ _ px1 px2 _, OSNum2 _ _ py1 py2 _) ->
--       runcurry $ \x1 x2 -> runcurry $ \y1 y2 ->
--       le (eqPrimS px1 py1) x1 y1 && le (eqPrimS px2 py2) x2 y2
--     (OSLogical1 px _, OSLogical1 py _) -> runcurry $ runcurry . le (eqPrimS px py)
--     (OSLogical2 px1 px2 _, OSLogical2 py1 py2 _) ->
--       runcurry $ \x1 x2 -> runcurry $ \y1 y2 ->
--       le (eqPrimS px1 py1) x1 y1 && le (eqPrimS px2 py2) x2 y2
--     (OSEq _ px1 px2 _, OSEq _ py1 py2 _) ->
--       runcurry $ \x1 x2 -> runcurry $ \y1 y2 ->
--       le (eqPrimS px1 py1) x1 y1 && le (eqPrimS px2 py2) x2 y2
--     (OSRel _ px1 px2 _, OSRel _ py1 py2 _) ->
--       runcurry $ \x1 x2 -> runcurry $ \y1 y2 ->
--       le (eqPrimS px1 py1) x1 y1 && le (eqPrimS px2 py2) x2 y2
--     (OSLookup (DArray (Index pi1) _), OSLookup (DArray (Index pi2) _)) ->
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
