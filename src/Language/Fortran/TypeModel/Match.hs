{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wall #-}

-- TODO: Complex Numbers

module Language.Fortran.TypeModel.Match where

import           Control.Monad                            ((>=>))
import           Data.Typeable

import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Singletons.TypeLits
import           GHC.TypeLits

import           Data.Vinyl                               hiding ((:~:))

import           Language.Fortran.TypeModel.Operator.Core
import           Language.Fortran.TypeModel.Singletons
import           Language.Fortran.TypeModel.Types
import           Language.Fortran.TypeModel.Util

--------------------------------------------------------------------------------
--  Matching on properties of types
--------------------------------------------------------------------------------

data MatchPrim a where
  MatchPrim :: Sing p -> Sing k -> Prim p k (PrimS a) -> MatchPrim (PrimS a)

-- | Checks if the given type is primitive, and if so returns a proof of that
-- fact.
matchPrim :: D a -> Maybe (MatchPrim a)
matchPrim = \case
  DPrim p -> case p of
    PInt8   -> Just (MatchPrim sing sing p)
    PInt16  -> Just (MatchPrim sing sing p)
    PInt32  -> Just (MatchPrim sing sing p)
    PInt64  -> Just (MatchPrim sing sing p)
    PFloat  -> Just (MatchPrim sing sing p)
    PDouble -> Just (MatchPrim sing sing p)
    PBool8  -> Just (MatchPrim sing sing p)
    PBool16 -> Just (MatchPrim sing sing p)
    PBool32 -> Just (MatchPrim sing sing p)
    PBool64 -> Just (MatchPrim sing sing p)
    PChar   -> Just (MatchPrim sing sing p)
  _ -> Nothing


data MakePrim p k where
  MakePrim :: Prim p k (PrimS a) -> MakePrim p k

-- | Tries to make a primitive type with the given precision and kind. Fails if
-- there is no primitive with the given combination.
makePrim :: Sing p -> Sing k -> Maybe (MakePrim p k)
makePrim = curry $ \case
  (SP8, SKInt)      -> Just $ MakePrim PInt8
  (SP16, SKInt)     -> Just $ MakePrim PInt16
  (SP32, SKInt)     -> Just $ MakePrim PInt32
  (SP64, SKInt)     -> Just $ MakePrim PInt64
  (SP32, SKReal)    -> Just $ MakePrim PFloat
  (SP64, SKReal)    -> Just $ MakePrim PDouble
  (SP8, SKLogical)  -> Just $ MakePrim PBool8
  (SP16, SKLogical) -> Just $ MakePrim PBool16
  (SP32, SKLogical) -> Just $ MakePrim PBool32
  (SP64, SKLogical) -> Just $ MakePrim PBool64
  (SP8, SKChar)     -> Just $ MakePrim PChar
  _ -> Nothing


data MatchNumType a where
  MatchNumType :: Sing p -> Sing k -> NumericKind k -> Prim p k a -> MatchNumType a

-- | Checks if the given type is numeric, and if so returns a proof of that
-- fact.
matchNumType :: D a -> Maybe (MatchNumType a)
matchNumType = matchPrim >=> \case
  MatchPrim sp SKInt p -> Just (MatchNumType sp SKInt NKInt p)
  MatchPrim sp SKReal p -> Just (MatchNumType sp SKReal NKReal p)
  _ -> Nothing


data MatchNumR a b where
  MatchNumR
    :: NumericKind k1 -> NumericKind k2
    -> Prim p1 k1 a -> Prim p2 k2 b
    -> Prim (PrecMax p1 p2) (KindMax k1 k2) (PrimS c)
    -> MatchNumR a b

-- | Checks if it is possible to perform a binary numeric operation on arguments
-- with the given respective types. If so, returns the type that would result
-- plus some more information about the types.
matchNumR :: D a -> D b -> Maybe (MatchNumR a b)
matchNumR = matchingWith2 matchNumType matchNumType $ \case
  (MatchNumType sp1 sk1 nk1 prim1, MatchNumType sp2 sk2 nk2 prim2) ->
    makePrim (sPrecMax sp1 sp2) (sKindMax sk1 sk2) <$$> \case
      MakePrim prim3 -> MatchNumR nk1 nk2 prim1 prim2 prim3


data MatchCompareR a b where
  MatchCompareR :: ComparableKinds k1 k2 -> Prim p1 k1 a -> Prim p2 k2 b -> MatchCompareR a b

-- | Checks if it is possible to perform a binary comparison (equality or
-- relational) operation on arguments with the given respective types. If so,
-- returns proof of that fact.
matchCompareR :: D a -> D b -> Maybe (MatchCompareR a b)
matchCompareR =
  (matchingWithBoth matchNumR $ Just . \case
      MatchNumR nk1 nk2 p1 p2 _ -> MatchCompareR (CKNum nk1 nk2) p1 p2
  ) `alt2`
  (matchingWith2 matchPrim matchPrim $ \case
      (MatchPrim _ SKLogical p1, MatchPrim _ SKLogical p2) ->
        Just (MatchCompareR CKBool p1 p2)
      (MatchPrim _ SKChar p1, MatchPrim _ SKChar p2) ->
        Just (MatchCompareR CKChar p1 p2)
      _ -> Nothing
  )

--------------------------------------------------------------------------------
--  Matching on operator result types
--------------------------------------------------------------------------------

data MatchOpR ok args where
  MatchOpR :: OpResult ok args result -> D result -> MatchOpR ok args

-- | Checks if it is possible to apply the given operator to the given
-- arguments, and if so returns a proof of that fact, packaged with information
-- about the result of applying the operator.
matchOpR :: Op (Length args) ok -> Rec D args -> Maybe (MatchOpR ok args)
matchOpR op argTypes =
  case argTypes of
    RNil -> case op of
      OpLit -> Nothing

    d1 :& RNil -> case op of
      OpNeg -> argsNumeric <$$> \case
        MatchNumType _ _ nk p :& RNil -> MatchOpR (ORNum1 nk p p) d1
      OpPos -> argsNumeric <$$> \case
        MatchNumType _ _ nk p :& RNil -> MatchOpR (ORNum1 nk p p) d1

      OpNot -> argsPrim >>= \case
        MatchPrim _ SKLogical p :& RNil -> Just $ MatchOpR (ORLogical1 p PBool8) (DPrim PBool8)
        _ -> Nothing

      -- In the deref case, we don't have access to a particular field to
      -- dereference, so there's nothing we can return.
      OpDeref -> Nothing

    d1 :& d2 :& RNil -> case op of
      OpAdd -> matchNumR d1 d2 <$$> \case
          MatchNumR nk1 nk2 p1 p2 p3 -> MatchOpR (ORNum2 nk1 nk2 p1 p2 p3) (DPrim p3)
      OpSub -> matchNumR d1 d2 <$$> \case
          MatchNumR nk1 nk2 p1 p2 p3 -> MatchOpR (ORNum2 nk1 nk2 p1 p2 p3) (DPrim p3)
      OpMul -> matchNumR d1 d2 <$$> \case
          MatchNumR nk1 nk2 p1 p2 p3 -> MatchOpR (ORNum2 nk1 nk2 p1 p2 p3) (DPrim p3)
      OpDiv -> matchNumR d1 d2 <$$> \case
          MatchNumR nk1 nk2 p1 p2 p3 -> MatchOpR (ORNum2 nk1 nk2 p1 p2 p3) (DPrim p3)

      OpAnd -> argsPrim >>= \case
        MatchPrim _ SKLogical p1 :& MatchPrim _ SKLogical p2 :& RNil ->
          Just $ MatchOpR (ORLogical2 p1 p2 PBool8) (DPrim PBool8)
        _ -> Nothing
      OpOr -> argsPrim >>= \case
        MatchPrim _ SKLogical p1 :& MatchPrim _ SKLogical p2 :& RNil ->
          Just $ MatchOpR (ORLogical2 p1 p2 PBool8) (DPrim PBool8)
        _ -> Nothing
      OpEquiv -> argsPrim >>= \case
        MatchPrim _ SKLogical p1 :& MatchPrim _ SKLogical p2 :& RNil ->
          Just $ MatchOpR (ORLogical2 p1 p2 PBool8) (DPrim PBool8)
        _ -> Nothing
      OpNotEquiv -> argsPrim >>= \case
        MatchPrim _ SKLogical p1 :& MatchPrim _ SKLogical p2 :& RNil ->
          Just $ MatchOpR (ORLogical2 p1 p2 PBool8) (DPrim PBool8)
        _ -> Nothing

      OpEq -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpR (OREq cmp p1 p2 PBool8) (DPrim PBool8)
      OpNE -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpR (OREq cmp p1 p2 PBool8) (DPrim PBool8)
      OpLT -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpR (ORRel cmp p1 p2 PBool8) (DPrim PBool8)
      OpLE -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpR (ORRel cmp p1 p2 PBool8) (DPrim PBool8)
      OpGT -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpR (ORRel cmp p1 p2 PBool8) (DPrim PBool8)
      OpGE -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpR (ORRel cmp p1 p2 PBool8) (DPrim PBool8)

      OpLookup -> traverse matchPrim (d1, d2) >>= \case
        (DArray (Index pi1) pv, MatchPrim _ _ pi2) -> case eqPrim pi1 pi2 of
          Just Refl -> Just $ MatchOpR (ORLookup d1) (primS pv DPrim)
          _         -> Nothing
        _ -> Nothing

    _ -> Nothing

  where
    argsNumeric = rtraverse matchNumType argTypes
    argsPrim = rtraverse matchPrim argTypes

--------------------------------------------------------------------------------
--  Equality of Fortran types
--------------------------------------------------------------------------------

eqSymbol :: forall n1 n2. SSymbol n1 -> SSymbol n2 -> Maybe (n1 :~: n2)
eqSymbol n1 n2 = withKnownSymbol n1 $ withKnownSymbol n2 $ sameSymbol (Proxy :: Proxy n1) (Proxy :: Proxy n2)

eqPrim :: Prim p1 k1 a -> Prim p2 k2 b -> Maybe ('(p1, k1, a) :~: '(p2, k2, b))
eqPrim = curry $ \case
  (PInt8, PInt8) -> Just Refl
  (PInt16, PInt16) -> Just Refl
  (PInt32, PInt32) -> Just Refl
  (PInt64, PInt64) -> Just Refl
  (PBool8, PBool8) -> Just Refl
  (PBool16, PBool16) -> Just Refl
  (PBool32, PBool32) -> Just Refl
  (PBool64, PBool64) -> Just Refl
  (PFloat, PFloat) -> Just Refl
  (PDouble, PDouble) -> Just Refl
  (PChar, PChar) -> Just Refl
  _ -> Nothing

eqRField :: RField a -> RField b -> Maybe (a :~: b)
eqRField = curry $ \case
  (RField n1 d1, RField n2 d2) ->
    case (eqSymbol n1 n2, eqD d1 d2) of
      (Just Refl, Just Refl) -> Just Refl
      _                      -> Nothing

eqRec :: Rec RField fs -> Rec RField gs -> Maybe (fs :~: gs)
eqRec = curry $ \case
  (RNil, RNil) -> Just Refl
  (h1 :& t1, h2 :& t2) ->
    case (eqRField h1 h2, eqRec t1 t2) of
      (Just Refl, Just Refl) -> Just Refl
      _                      -> Nothing
  _ -> Nothing

eqD :: D a -> D b -> Maybe (a :~: b)
eqD = curry $ \case
  (DPrim p1, DPrim p2) ->
    case eqPrim p1 p2 of
      Just Refl -> Just Refl
      _         -> Nothing
  (DArray (Index i1) p1, DArray (Index i2) p2) ->
    case (eqPrim i1 i2, eqPrim p1 p2) of
      (Just Refl, Just Refl) -> Just Refl
      _                      -> Nothing
  (DData n1 r1, DData n2 r2) ->
    case (eqSymbol n1 n2, eqRec r1 r2) of
      (Just Refl, Just Refl) -> Just Refl
      _                      -> Nothing
  _ -> Nothing

dcast :: D a -> D b -> f a -> Maybe (f b)
dcast d1 d2 = case eqD d1 d2 of
  Just Refl -> Just
  _         -> const Nothing
