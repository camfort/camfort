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

module Language.Fortran.Model.Op.Core.Match where

import           Control.Monad                       ((>=>))
import           Data.Typeable

import           Control.Lens

import           Data.Singletons
import           Data.Singletons.Prelude.List

import           Data.Vinyl                          hiding ((:~:), Field)

import           Language.Fortran.Model.Op.Core.Core
import           Language.Fortran.Model.Singletons
import           Language.Fortran.Model.Types
import           Language.Fortran.Model.Types.Match
import           Language.Fortran.Model.Util


data MatchNumType a where
  MatchNumType :: Sing p -> Sing k -> NumericBasicType k -> Prim p k a -> MatchNumType (PrimS a)

-- | Checks if the given type is numeric, and if so returns a proof of that
-- fact.
matchNumType :: D a -> Maybe (MatchNumType a)
matchNumType = matchPrimD >=> \case
  MatchPrimD (MatchPrim sp SBTInt) p -> Just (MatchNumType sp SBTInt NBTInt p)
  MatchPrimD (MatchPrim sp SBTReal) p -> Just (MatchNumType sp SBTReal NBTReal p)
  _ -> Nothing


data MatchNumR a b where
  MatchNumR
    :: NumericBasicType k1 -> NumericBasicType k2
    -> Prim p1 k1 a -> Prim p2 k2 b
    -> Prim (PrecMax p1 p2) (BasicTypeMax k1 k2) c
    -> MatchNumR (PrimS a) (PrimS b)

-- | Checks if it is possible to perform a binary numeric operation on arguments
-- with the given respective types. If so, returns the type that would result
-- plus some more information about the types.
matchNumR :: D a -> D b -> Maybe (MatchNumR a b)
matchNumR = matchingWith2 matchNumType matchNumType $ \case
  (MatchNumType sp1 sk1 nk1 prim1, MatchNumType sp2 sk2 nk2 prim2) ->
    makePrim (sPrecMax sp1 sp2) (sBasicTypeMax sk1 sk2) <$$> \case
      MakePrim prim3 -> MatchNumR nk1 nk2 prim1 prim2 prim3

primCeil :: Prim p1 k1 a -> Prim p2 k2 b -> Maybe (MakePrim (PrecMax p1 p2) (BasicTypeMax k1 k2))
primCeil prim1 prim2 = case (matchPrim prim1, matchPrim prim2) of
  (MatchPrim p1 k1, MatchPrim p2 k2) -> makePrim (sPrecMax p1 p2) (sBasicTypeMax k1 k2)


data MatchCompareR a b where
  MatchCompareR :: ComparableBasicTypes k1 k2 -> Prim p1 k1 a -> Prim p2 k2 b -> MatchCompareR (PrimS a) (PrimS b)

-- | Checks if it is possible to perform a binary comparison (equality or
-- relational) operation on arguments with the given respective types. If so,
-- returns proof of that fact.
matchCompareR :: D a -> D b -> Maybe (MatchCompareR a b)
matchCompareR =
  (matchingWithBoth matchNumR $ Just . \case
      MatchNumR nk1 nk2 p1 p2 _ -> MatchCompareR (CBTNum nk1 nk2) p1 p2
  ) `altf2`
  (matchingWith2 matchPrimD matchPrimD $ \case
      (MatchPrimD (MatchPrim _ SBTLogical) p1, MatchPrimD (MatchPrim _ SBTLogical) p2) ->
        Just (MatchCompareR CBTBool p1 p2)
      (MatchPrimD (MatchPrim _ SBTChar) p1, MatchPrimD (MatchPrim _ SBTChar) p2) ->
        Just (MatchCompareR CBTChar p1 p2)
      _ -> Nothing
  )

--------------------------------------------------------------------------------
--  Matching on operator result types
--------------------------------------------------------------------------------

data MatchOpSpec ok args where
  MatchOpSpec :: OpSpec ok args result -> D result -> MatchOpSpec ok args

-- | Checks if it is possible to apply the given operator to the given
-- arguments, and if so returns a proof of that fact, packaged with information
-- about the result of applying the operator.
matchOpSpec :: Op (Length args) ok -> Rec D args -> Maybe (MatchOpSpec ok args)
matchOpSpec operator argTypes =
  case argTypes of
    RNil -> case operator of
      OpLit -> Nothing

    d1 :& RNil -> case operator of
      OpNeg -> argsNumeric <$$> \case
        MatchNumType _ _ nk p :& RNil -> MatchOpSpec (OSNum1 nk p p) d1
      OpPos -> argsNumeric <$$> \case
        MatchNumType _ _ nk p :& RNil -> MatchOpSpec (OSNum1 nk p p) d1

      OpNot -> argsPrim >>= \case
        MatchPrimD (MatchPrim _ SBTLogical) p :& RNil -> Just $ MatchOpSpec (OSLogical1 p PBool8) (DPrim PBool8)
        _ -> Nothing

      -- In the deref case, we don't have access to a particular field to
      -- dereference, so there's nothing we can return.
      OpDeref -> Nothing

    d1 :& d2 :& RNil -> case operator of
      OpAdd -> matchNumR d1 d2 <$$> \case
          MatchNumR nk1 nk2 p1 p2 p3 -> MatchOpSpec (OSNum2 nk1 nk2 p1 p2 p3) (DPrim p3)
      OpSub -> matchNumR d1 d2 <$$> \case
          MatchNumR nk1 nk2 p1 p2 p3 -> MatchOpSpec (OSNum2 nk1 nk2 p1 p2 p3) (DPrim p3)
      OpMul -> matchNumR d1 d2 <$$> \case
          MatchNumR nk1 nk2 p1 p2 p3 -> MatchOpSpec (OSNum2 nk1 nk2 p1 p2 p3) (DPrim p3)
      OpDiv -> matchNumR d1 d2 <$$> \case
          MatchNumR nk1 nk2 p1 p2 p3 -> MatchOpSpec (OSNum2 nk1 nk2 p1 p2 p3) (DPrim p3)

      OpAnd -> argsPrim >>= \case
        MatchPrimD (MatchPrim _ SBTLogical) p1 :& MatchPrimD (MatchPrim _ SBTLogical) p2 :& RNil ->
          Just $ MatchOpSpec (OSLogical2 p1 p2 PBool8) (DPrim PBool8)
        _ -> Nothing
      OpOr -> argsPrim >>= \case
        MatchPrimD (MatchPrim _ SBTLogical) p1 :& MatchPrimD (MatchPrim _ SBTLogical) p2 :& RNil ->
          Just $ MatchOpSpec (OSLogical2 p1 p2 PBool8) (DPrim PBool8)
        _ -> Nothing
      OpEquiv -> argsPrim >>= \case
        MatchPrimD (MatchPrim _ SBTLogical) p1 :& MatchPrimD (MatchPrim _ SBTLogical) p2 :& RNil ->
          Just $ MatchOpSpec (OSLogical2 p1 p2 PBool8) (DPrim PBool8)
        _ -> Nothing
      OpNotEquiv -> argsPrim >>= \case
        MatchPrimD (MatchPrim _ SBTLogical) p1 :& MatchPrimD (MatchPrim _ SBTLogical) p2 :& RNil ->
          Just $ MatchOpSpec (OSLogical2 p1 p2 PBool8) (DPrim PBool8)
        _ -> Nothing

      OpEq -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpSpec (OSEq cmp p1 p2 PBool8) (DPrim PBool8)
      OpNE -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpSpec (OSEq cmp p1 p2 PBool8) (DPrim PBool8)
      OpLT -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpSpec (OSRel cmp p1 p2 PBool8) (DPrim PBool8)
      OpLE -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpSpec (OSRel cmp p1 p2 PBool8) (DPrim PBool8)
      OpGT -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpSpec (OSRel cmp p1 p2 PBool8) (DPrim PBool8)
      OpGE -> matchCompareR d1 d2 <$$> \case
        MatchCompareR cmp p1 p2 -> MatchOpSpec (OSRel cmp p1 p2 PBool8) (DPrim PBool8)

      OpLookup -> with (d1, d2) $ traverseOf _2 matchPrimD >=> \case
        (DArray (Index pi1) av, MatchPrimD _ pi2) -> case eqPrim pi1 pi2 of
          Just Refl -> Just $ MatchOpSpec (OSLookup d1) (dArrValue av)
          _         -> Nothing
        _ -> Nothing

    _ -> Nothing

  where
    argsNumeric = rtraverse matchNumType argTypes
    argsPrim = rtraverse matchPrimD argTypes
