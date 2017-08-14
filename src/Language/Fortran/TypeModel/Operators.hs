{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wall #-}

module Language.Fortran.TypeModel.Operators
  (
  -- * Model of Fortran Operators
    FortranOp(..)
  , OpKind(..)
  , Op1(..)
  , Op2(..)
  , Op1Result(..)
  , Op2Result(..)
  -- * Type machinery
  , OpKindSing(..)
  , SingOpKind(..)
  , SomeOp1Result(..)
  , SomeOp2Result(..)
  , getOp1Result
  , getOp2Result
  ) where

import           Data.Typeable ((:~:)(..))

import           Data.SBV                             hiding (KReal, Kind)

import           Language.Expression

import           Language.Fortran.TypeModel.Basic
import           Language.Fortran.TypeModel.Machinery

--------------------------------------------------------------------------------
--  Model of Fortran operators
--------------------------------------------------------------------------------

data OpKind
  = OpNum
  | OpEquality
  | OpRelational
  | OpLogical
  | OpProp
  -- ^ A dummy operator type for embedding Fortran expressions in propositions
  deriving (Show)

data Op1 k where
  OpPos, OpNeg :: Op1 'OpNum
  OpNot :: Op1 'OpLogical

  OpEmbedProp :: Op1 'OpProp

deriving instance Show (Op1 k)

data Op2 k where
  OpAdd, OpSub, OpMul, OpDiv :: Op2 'OpNum
  OpEq, OpNE :: Op2 'OpEquality
  OpLT, OpLE, OpGT, OpGE :: Op2 'OpRelational
  OpAnd, OpOr, OpEquiv, OpNotEquiv :: Op2 'OpLogical

deriving instance Show (Op2 k)

data Op1Result ok (p1 :: Precision) k1 (p2 :: Precision) k2 where
  Op1Num :: Numeric k => Op1Result 'OpNum p k p k

  Op1Logical :: Op1Result 'OpLogical p 'KLogical p 'KLogical

  Op1Prop :: Op1Result 'OpProp p 'KLogical 'P64 'KProp


deriving instance Show (Op1Result ok p1 k1 p2 k2)

data Op2Result ok p1 k1 p2 k2 p3 k3 where
  Op2Num
    :: (PrecSing p1, PrecSing p2, KindSing k1, KindSing k2)
    => Op2Result 'OpNum p1 k1 p2 k2 (PrecMax p1 p2) (NumKindMax k1 k2)
  Op2Eq
    :: (PrecSing p1, PrecSing p2, KindSing k1, KindSing k2, Comparable k1 k2)
    => Op2Result 'OpEquality p1 k1 p2 k2 'P8 'KLogical
  Op2Rel
    :: (PrecSing p1, PrecSing p2, KindSing k1, KindSing k2, Comparable k1 k2)
    => Op2Result 'OpRelational p1 k1 p2 k2 'P8 'KLogical
  Op2Logical
    :: (PrecSing p1, PrecSing p2)
    => Op2Result 'OpLogical p1 'KLogical p2 'KLogical
                            (PrecMax p1 p2) 'KLogical


data FortranOp t a where
  OpLit
    :: (PrecSing p, KindSing k)
    => D p k a -> a -> FortranOp t a
  Op1
    :: (PrecSing p1, PrecSing p2, KindSing k1, KindSing k2)
    => Op1 ok -> Op1Result ok p1 k1 p2 k2
    -> D p1 k1 a -> D p2 k2 b
    -> t a -> FortranOp t b
  Op2
    :: (PrecSing p1, PrecSing p2, PrecSing p3, KindSing k1, KindSing k2, KindSing k3)
    => Op2 ok -> Op2Result ok p1 k1 p2 k2 p3 k3
    -> D p1 k1 a -> D p2 k2 b -> D p3 k3 c
    -> t a -> t b -> FortranOp t c

instance Operator FortranOp where
  htraverseOp f = \case
    OpLit d x -> pure (OpLit d x)
    Op1 op opr d1 d2 x -> Op1 op opr d1 d2 <$> f x
    Op2 op opr d1 d2 d3 x y -> Op2 op opr d1 d2 d3 <$> f x <*> f y

instance (Applicative f) => EvalOp f SBV FortranOp where
  evalOp f = \case
    OpLit d x -> pure (dToSBV d x)

    Op1 OpEmbedProp opr d1 d2 x ->
      case matchOp1Result opr d1 d2 of
        MOp1RProp -> toSBool <$> f x

    Op1 OpPos opr d1 d2 x -> withOp1Num (matchOp1Result opr d1 d2) id <$> f x
    Op1 OpNeg opr d1 d2 x -> withOp1Num (matchOp1Result opr d1 d2) negate <$> f x
    Op1 OpNot opr d1 d2 x -> withOp1Logical (matchOp1Result opr d1 d2) bnot <$> f x

    Op2 OpAdd opr d1 d2 d3 x y ->
      withOp2Num (matchOp2Result opr d1 d2 d3) (+) <$> f x <*> f y
    Op2 OpSub opr d1 d2 d3 x y ->
      withOp2Num (matchOp2Result opr d1 d2 d3) (-) <$> f x <*> f y
    Op2 OpMul opr d1 d2 d3 x y ->
      withOp2Num (matchOp2Result opr d1 d2 d3) (*) <$> f x <*> f y
    Op2 OpDiv opr d1 d2 d3 x y ->
      withOp2Num (matchOp2Result opr d1 d2 d3) udiv <$> f x <*> f y

    Op2 OpEq opr d1 d2 d3 x y ->
      withOp2Eq (matchOp2Result opr d1 d2 d3) (.==) <$> f x <*> f y
    Op2 OpNE opr d1 d2 d3 x y ->
      withOp2Eq (matchOp2Result opr d1 d2 d3) (./=) <$> f x <*> f y

    Op2 OpLT opr d1 d2 d3 x y ->
      withOp2Rel (matchOp2Result opr d1 d2 d3) (.<) <$> f x <*> f y
    Op2 OpGT opr d1 d2 d3 x y ->
      withOp2Rel (matchOp2Result opr d1 d2 d3) (.>) <$> f x <*> f y
    Op2 OpLE opr d1 d2 d3 x y ->
      withOp2Rel (matchOp2Result opr d1 d2 d3) (.<=) <$> f x <*> f y
    Op2 OpGE opr d1 d2 d3 x y ->
      withOp2Rel (matchOp2Result opr d1 d2 d3) (.>=) <$> f x <*> f y

    Op2 OpAnd opr d1 d2 d3 x y ->
      withOp2Logical (matchOp2Result opr d1 d2 d3) (&&&) <$> f x <*> f y
    Op2 OpOr opr d1 d2 d3 x y ->
      withOp2Logical (matchOp2Result opr d1 d2 d3) (|||) <$> f x <*> f y
    Op2 OpEquiv opr d1 d2 d3 x y ->
      withOp2Logical (matchOp2Result opr d1 d2 d3) (<=>) <$> f x <*> f y
    Op2 OpNotEquiv opr d1 d2 d3 x y ->
      withOp2Logical (matchOp2Result opr d1 d2 d3) (\a b -> bnot (a <=> b)) <$> f x <*> f y

--------------------------------------------------------------------------------
--  Constraint Machinery
--------------------------------------------------------------------------------

-- | The result of matching on a unary operator result. Packages constraints
-- that will always be satisfied by the operands and result.
data MatchOp1Result ok a b where
  MOp1RNum :: (Num a, SymWord a, UDiv (SBV a)) => MatchOp1Result 'OpNum a a
  MOp1RLogical :: SymBool a => MatchOp1Result 'OpLogical a a
  MOp1RProp :: SymBool a => MatchOp1Result 'OpProp a Bool

matchOp1Result :: Op1Result ok p1 k1 p2 k2 -> D p1 k1 a -> D p2 k2 b -> MatchOp1Result ok a b
matchOp1Result Op1Num d1 d2 =
  case (dequiv d1 d2, matchKind d1, matchKind d2) of
    (Refl, MKInt, MKInt) -> MOp1RNum
    (Refl, MKReal, MKReal) -> MOp1RNum
    _ -> error "impossible"

matchOp1Result Op1Logical d1 d2 =
  case (dequiv d1 d2, matchKind d1, matchKind d2) of
    (Refl, MKLogical, MKLogical) -> MOp1RLogical

matchOp1Result Op1Prop d1 d2 =
  case (matchKind d1, matchKind d2) of
    (MKLogical, MKProp) -> MOp1RProp


-- | The result of matching on a binary operator result. Packages constraints
-- that will always be satisfied by the operands and result.
data MatchOp2Result ok a b c where
  MOp2RNum
    :: (Num c, SymWord c, UDiv (SBV c))
    => Convert a c -> Convert b c -> MatchOp2Result 'OpNum a b c

  MOp2REq
    :: (SymBool c, EqSymbolic (SBV d))
    => Convert a d -> Convert b d -> MatchOp2Result 'OpEquality a b c

  MOp2RRel
    :: (SymBool c, OrdSymbolic (SBV d))
    => Convert a d -> Convert b d -> MatchOp2Result 'OpRelational a b c

  MOp2RLogical
    :: (SymBool c)
    => Convert a c -> Convert b c -> MatchOp2Result 'OpLogical a b c

matchOp2Result
  :: (PrecSing p1, PrecSing p2, PrecSing p3, KindSing k1, KindSing k2, KindSing k3)
  => Op2Result ok p1 k1 p2 k2 p3 k3
  -> D p1 k1 a -> D p2 k2 b -> D p3 k3 c
  -> MatchOp2Result ok a b c
matchOp2Result Op2Num d1 d2 d3 =
  case (matchKind d1, matchKind d2, matchKind d3) of
    (MKInt, MKInt, MKInt) -> uncurry MOp2RNum (numUpcast d1 d2 d3)
    (MKInt, MKReal, MKReal) -> uncurry MOp2RNum (numUpcast d1 d2 d3)
    (MKReal, MKInt, MKReal) -> uncurry MOp2RNum (numUpcast d1 d2 d3)
    (MKReal, MKReal, MKReal) -> uncurry MOp2RNum (numUpcast d1 d2 d3)
    _ -> error "impossible"

matchOp2Result Op2Eq d1 d2 d3 =
  case (matchKind d1, matchKind d2, matchKind d3) of
    (MKInt, MKInt, MKLogical) ->
      case maxD d1 d2 of
        DWithParams d4 -> case matchKind d4 of
          MKInt -> uncurry MOp2REq (numUpcast d1 d2 d4)
    (MKInt, MKReal, MKLogical) ->
      case maxD d1 d2 of
        DWithParams d4 -> case matchKind d4 of
          MKReal -> uncurry MOp2REq (numUpcast d1 d2 d4)
    (MKReal, MKInt, MKLogical) ->
      case maxD d1 d2 of
        DWithParams d4 -> case matchKind d4 of
          MKReal -> uncurry MOp2REq (numUpcast d1 d2 d4)
    (MKReal, MKReal, MKLogical) ->
      case maxD d1 d2 of
        DWithParams d4 -> case matchKind d4 of
          MKReal -> uncurry MOp2REq (numUpcast d1 d2 d4)
    (MKChar, MKChar, MKLogical) -> MOp2REq id id
    _ -> error "impossible"

matchOp2Result Op2Rel d1 d2 d3 =
  case (matchKind d1, matchKind d2, matchKind d3) of
    (MKInt, MKInt, MKLogical) ->
      case maxD d1 d2 of
        DWithParams d4 -> case matchKind d4 of
          MKInt -> uncurry MOp2RRel (numUpcast d1 d2 d4)
    (MKInt, MKReal, MKLogical) ->
      case maxD d1 d2 of
        DWithParams d4 -> case matchKind d4 of
          MKReal -> uncurry MOp2RRel (numUpcast d1 d2 d4)
    (MKReal, MKInt, MKLogical) ->
      case maxD d1 d2 of
        DWithParams d4 -> case matchKind d4 of
          MKReal -> uncurry MOp2RRel (numUpcast d1 d2 d4)
    (MKReal, MKReal, MKLogical) ->
      case maxD d1 d2 of
        DWithParams d4 -> case matchKind d4 of
          MKReal -> uncurry MOp2RRel (numUpcast d1 d2 d4)
    (MKChar, MKChar, MKLogical) -> MOp2RRel id id
    _ -> error "impossible"

matchOp2Result Op2Logical d1 d2 d3 =
  case (matchKind d1, matchKind d2, matchKind d3) of
    (MKLogical, MKLogical, MKLogical) ->
      MOp2RLogical (fromSBool . toSBool) (fromSBool . toSBool)

withOp1Num
  :: MatchOp1Result 'OpNum a b
  -> ((Num a, SymWord a) => SBV a -> SBV a)
  -> SBV a -> SBV b
withOp1Num MOp1RNum f = f

withOp1Logical
  :: MatchOp1Result 'OpLogical a b
  -> (SBool -> SBool)
  -> SBV a -> SBV b
withOp1Logical MOp1RLogical f = fromSBool . f . toSBool

withOp2Num
  :: MatchOp2Result 'OpNum a b c
  -> ((Num c, SymWord c, UDiv (SBV c)) => SBV c -> SBV c -> SBV c)
  -> SBV a -> SBV b -> SBV c
withOp2Num (MOp2RNum ac bc) f x y = f (ac x) (bc y)

withOp2Eq
  :: MatchOp2Result 'OpEquality a b c
  -> (forall x. (EqSymbolic x) => x -> x -> SBool)
  -> SBV a -> SBV b -> SBV c
withOp2Eq (MOp2REq ad bd) f x y = fromSBool $ f (ad x) (bd y)

withOp2Rel
  :: MatchOp2Result 'OpRelational a b c
  -> (forall x. (OrdSymbolic x) => x -> x -> SBool)
  -> SBV a -> SBV b -> SBV c
withOp2Rel (MOp2RRel ad bd) f x y = fromSBool $ f (ad x) (bd y)

withOp2Logical
  :: MatchOp2Result 'OpLogical a b c
  -> (SBool -> SBool -> SBool)
  -> SBV a -> SBV b -> SBV c
withOp2Logical (MOp2RLogical ac bc) f x y =
  fromSBool $ f (toSBool $ ac x) (toSBool $ bc y)


-- TODO: Fill in errors
dToSBV :: D p k a -> a -> SBV a
dToSBV DInt8 = fromIntegral
dToSBV DInt16 = fromIntegral
dToSBV DInt32 = fromIntegral
dToSBV DInt64 = fromIntegral
dToSBV DFloat = error "float to SBV"
dToSBV DDouble = error "double to SBV"
dToSBV DBool8 = fromBool . toBool
dToSBV DBool16 = fromBool . toBool
dToSBV DBool32 = fromBool . toBool
dToSBV DBool64 = fromBool . toBool
dToSBV DChar = error "char to SBV"
dToSBV DProp = fromBool

--------------------------------------------------------------------------------
--  Existential Results
--------------------------------------------------------------------------------

data SingOpKind ok where
  SingOpNum :: SingOpKind 'OpNum
  SingOpEquality :: SingOpKind 'OpEquality
  SingOpRelational :: SingOpKind 'OpRelational
  SingOpLogical :: SingOpKind 'OpLogical
  SingOpProp :: SingOpKind 'OpProp

class OpKindSing ok where
  opKindSing :: proxy ok -> SingOpKind ok

instance OpKindSing 'OpNum where opKindSing _ = SingOpNum
instance OpKindSing 'OpEquality where opKindSing _ = SingOpEquality
instance OpKindSing 'OpRelational where opKindSing _ = SingOpRelational
instance OpKindSing 'OpLogical where opKindSing _ = SingOpLogical
instance OpKindSing 'OpProp where opKindSing _ = SingOpProp


data SomeOp1Result ok p1 k1 where
  SomeOp1Result :: Op1Result ok p1 k1 p2 k2 -> SomeOp1Result ok p1 k1

data SomeOp2Result ok p1 k1 p2 k2 where
  SomeOp2Result :: Op2Result ok p1 k1 p2 k2 p3 k3 -> SomeOp2Result ok p1 k1 p2 k2


getOp1Result :: OpKindSing ok => proxy ok -> D p1 k1 a -> Maybe (SomeOp1Result ok p1 k1)
getOp1Result op d =
  case opKindSing op of
    SingOpNum ->
      case matchKind d of
        MKInt -> Just (SomeOp1Result Op1Num)
        MKReal -> Just (SomeOp1Result Op1Num)
        _ -> Nothing
    SingOpLogical ->
      case matchKind d of
        MKLogical -> Just (SomeOp1Result Op1Logical)
        _ -> Nothing
    SingOpProp ->
      case matchKind d of
        MKLogical -> Just (SomeOp1Result Op1Prop)
        _ -> Nothing
    _ -> Nothing


getOp2Result
  :: (OpKindSing ok, PrecSing p1, PrecSing p2, KindSing k1, KindSing k2)
  => proxy ok -> D p1 k1 a -> D p2 k2 b -> Maybe (SomeOp2Result ok p1 k1 p2 k2)
getOp2Result op d1 d2 =
  case opKindSing op of
    SingOpNum ->
      case (matchKind d1, matchKind d2) of
        (MKInt, MKInt) -> Just (SomeOp2Result Op2Num)
        (MKInt, MKReal) -> Just (SomeOp2Result Op2Num)
        (MKReal, MKInt) -> Just (SomeOp2Result Op2Num)
        (MKReal, MKReal) -> Just (SomeOp2Result Op2Num)
        _ -> Nothing
    SingOpEquality ->
      case (matchKind d1, matchKind d2) of
        (MKInt, MKInt) -> Just (SomeOp2Result Op2Eq)
        (MKInt, MKReal) -> Just (SomeOp2Result Op2Eq)
        (MKReal, MKInt) -> Just (SomeOp2Result Op2Eq)
        (MKReal, MKReal) -> Just (SomeOp2Result Op2Eq)
        (MKChar, MKChar) -> Just (SomeOp2Result Op2Eq)
        _ -> Nothing
    SingOpRelational ->
      case (matchKind d1, matchKind d2) of
        (MKInt, MKInt) -> Just (SomeOp2Result Op2Rel)
        (MKInt, MKReal) -> Just (SomeOp2Result Op2Rel)
        (MKReal, MKInt) -> Just (SomeOp2Result Op2Rel)
        (MKReal, MKReal) -> Just (SomeOp2Result Op2Rel)
        (MKChar, MKChar) -> Just (SomeOp2Result Op2Rel)
        _ -> Nothing
    SingOpLogical ->
      case (matchKind d1, matchKind d2) of
        (MKLogical, MKLogical) -> Just (SomeOp2Result Op2Logical)
        _ -> Nothing
    _ -> Nothing
