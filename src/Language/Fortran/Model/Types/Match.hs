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
{-# LANGUAGE CPP                        #-}

{-# OPTIONS_GHC -Wall #-}

-- TODO: Complex Numbers

module Language.Fortran.Model.Types.Match where

import           Data.Typeable

import           Data.Singletons
import           GHC.TypeLits

#if MIN_VERSION_singletons(3,0,0)
import           GHC.TypeLits.Singletons
#else
import           Data.Singletons.TypeLits
#endif

import           Data.Vinyl                        hiding ((:~:), Field)

import           Language.Fortran.Model.Singletons
import           Language.Fortran.Model.Types

--------------------------------------------------------------------------------
--  Matching on properties of types
--------------------------------------------------------------------------------

data MatchPrim p k a where
  MatchPrim :: Sing p -> Sing k -> MatchPrim p k a

matchPrim :: Prim p k a -> MatchPrim p k a
matchPrim = \case
  PInt8   -> MatchPrim sing sing
  PInt16  -> MatchPrim sing sing
  PInt32  -> MatchPrim sing sing
  PInt64  -> MatchPrim sing sing
  PFloat  -> MatchPrim sing sing
  PDouble -> MatchPrim sing sing
  PBool8  -> MatchPrim sing sing
  PBool16 -> MatchPrim sing sing
  PBool32 -> MatchPrim sing sing
  PBool64 -> MatchPrim sing sing
  PChar   -> MatchPrim sing sing


data MatchPrimD a where
  MatchPrimD :: MatchPrim p k a -> Prim p k a -> MatchPrimD (PrimS a)

-- | Checks if the given type is primitive, and if so returns a proof of that
-- fact.
matchPrimD :: D a -> Maybe (MatchPrimD a)
matchPrimD = \case
  DPrim p -> Just (MatchPrimD (matchPrim p) p)
  _ -> Nothing


data MakePrim p k where
  MakePrim :: Prim p k a -> MakePrim p k

-- | Tries to make a primitive type with the given precision and kind. Fails if
-- there is no primitive with the given combination.
makePrim :: Sing p -> Sing k -> Maybe (MakePrim p k)
makePrim = curry $ \case
  (SP8, SBTInt)      -> Just $ MakePrim PInt8
  (SP16, SBTInt)     -> Just $ MakePrim PInt16
  (SP32, SBTInt)     -> Just $ MakePrim PInt32
  (SP64, SBTInt)     -> Just $ MakePrim PInt64
  (SP32, SBTReal)    -> Just $ MakePrim PFloat
  (SP64, SBTReal)    -> Just $ MakePrim PDouble
  (SP8, SBTLogical)  -> Just $ MakePrim PBool8
  (SP16, SBTLogical) -> Just $ MakePrim PBool16
  (SP32, SBTLogical) -> Just $ MakePrim PBool32
  (SP64, SBTLogical) -> Just $ MakePrim PBool64
  (SP8, SBTChar)     -> Just $ MakePrim PChar
  _ -> Nothing

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


eqField :: (forall x y. f x -> g y -> Maybe (x :~: y)) -> Field f p1 -> Field g p2 -> Maybe (p1 :~: p2)
eqField eqVals = curry $ \case
  (Field n1 x1, Field n2 x2) ->
    case (eqSymbol n1 n2, eqVals x1 x2) of
      (Just Refl, Just Refl) -> Just Refl
      _                      -> Nothing


eqRec :: (forall a b. t a -> t b -> Maybe (a :~: b)) -> Rec (Field t) fs -> Rec (Field t) gs -> Maybe (fs :~: gs)
eqRec eqInside = curry $ \case
  (RNil, RNil) -> Just Refl
  (h1 :& t1, h2 :& t2) ->
    case (eqField eqInside h1 h2, eqRec eqInside t1 t2) of
      (Just Refl, Just Refl) -> Just Refl
      _                      -> Nothing
  _ -> Nothing


eqArrValue :: ArrValue a -> ArrValue b -> Maybe (a :~: b)
eqArrValue a1 a2 = eqD (dArrValue a1) (dArrValue a2)


eqD :: D a -> D b -> Maybe (a :~: b)
eqD = curry $ \case
  (DPrim p1, DPrim p2) ->
    case eqPrim p1 p2 of
      Just Refl -> Just Refl
      _         -> Nothing
  (DArray (Index i1) av1, DArray (Index i2) av2) ->
    case (eqPrim i1 i2, eqArrValue av1 av2) of
      (Just Refl, Just Refl) -> Just Refl
      _ -> Nothing
  (DData n1 r1, DData n2 r2) ->
    case (eqSymbol n1 n2, eqRec eqD r1 r2) of
      (Just Refl, Just Refl) -> Just Refl
      _                      -> Nothing
  _ -> Nothing


dcast :: D a -> D b -> f a -> Maybe (f b)
dcast d1 d2 = case eqD d1 d2 of
  Just Refl -> Just
  _         -> const Nothing
