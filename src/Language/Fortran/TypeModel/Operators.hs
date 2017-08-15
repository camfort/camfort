{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TemplateHaskell               #-}
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
  , SomeOp1Result(..)
  , SomeOp2Result(..)
  , getOp1Result
  , getOp2Result
  , getDFromOp1Result
  , getDFromOp2Result
  ) where

import           Data.Typeable ((:~:)(..))
import           Data.Functor.Classes

import           Data.SBV                             hiding (KReal, Kind)
import           Data.Singletons

import           Language.Expression
import           Language.Expression.Pretty

import           Language.Fortran.TypeModel.Basic
import           Language.Fortran.TypeModel.Singletons
import           Language.Fortran.TypeModel.Machinery
import           Language.Fortran.TypeModel.SBV

--------------------------------------------------------------------------------
--  Model of Fortran operators
--------------------------------------------------------------------------------

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
    :: (Numeric k1, Numeric k2)
    => Op2Result 'OpNum p1 k1 p2 k2 (PrecMax p1 p2) (NumKindMax k1 k2)
  Op2Eq
    :: (Comparable k1 k2)
    => Op2Result 'OpEquality p1 k1 p2 k2 'P8 'KLogical
  Op2Rel
    :: (Comparable k1 k2)
    => Op2Result 'OpRelational p1 k1 p2 k2 'P8 'KLogical
  Op2Logical
    :: Op2Result 'OpLogical p1 'KLogical p2 'KLogical
                            (PrecMax p1 p2) 'KLogical


data FortranOp t a where
  OpLit
    :: (SingI p, SingI k)
    => D p k a -> a -> FortranOp t a
  Op1
    :: (SingI p1, SingI p2, SingI k1, SingI k2)
    => Op1 ok -> Op1Result ok p1 k1 p2 k2
    -> D p1 k1 a -> D p2 k2 b
    -> t a -> FortranOp t b
  Op2
    :: (SingI p1, SingI p2, SingI p3, SingI k1, SingI k2, SingI k3)
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
    OpLit d x -> pure (literalD d x)

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
  MOp1RNum :: (HasRepr a, Num a, UDiv (SBV a)) => MatchOp1Result 'OpNum a a
  MOp1RLogical :: (HasRepr a, SymBool a) => MatchOp1Result 'OpLogical a a
  MOp1RProp :: (HasRepr a, SymBool a) => MatchOp1Result 'OpProp a Bool

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
    :: (HasRepr c, Num c, UDiv (SBV c))
    => Convert a c -> Convert b c -> MatchOp2Result 'OpNum a b c

  MOp2REq
    :: (HasRepr c, SymBool c, EqSymbolic (SBV d))
    => Convert a d -> Convert b d -> MatchOp2Result 'OpEquality a b c

  MOp2RRel
    :: (HasRepr c, SymBool c, OrdSymbolic (SBV d))
    => Convert a d -> Convert b d -> MatchOp2Result 'OpRelational a b c

  MOp2RLogical
    :: (SymBool a, SymBool b, SymBool c)
    => Convert a c -> Convert b c -> MatchOp2Result 'OpLogical a b c

matchOp2Result
  :: (SingI p1, SingI p2, SingI p3, SingI k1, SingI k2, SingI k3)
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
  -> ((HasRepr a, Num a) => SBV a -> SBV a)
  -> SBV a -> SBV b
withOp1Num MOp1RNum f = f

withOp1Logical
  :: MatchOp1Result 'OpLogical a b
  -> (SBool -> SBool)
  -> SBV a -> SBV b
withOp1Logical MOp1RLogical f = fromSBool . f . toSBool

withOp2Num
  :: MatchOp2Result 'OpNum a b c
  -> ((HasRepr c, Num c, UDiv (SBV c)) => SBV c -> SBV c -> SBV c)
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


--------------------------------------------------------------------------------
--  Existential Results
--------------------------------------------------------------------------------

data SomeOp1Result ok p1 k1 where
  SomeOp1Result :: (SingI p2, SingI k2) => Op1Result ok p1 k1 p2 k2 -> SomeOp1Result ok p1 k1

data SomeOp2Result ok p1 k1 p2 k2 where
  SomeOp2Result :: (SingI p3, SingI k3) => Op2Result ok p1 k1 p2 k2 p3 k3 -> SomeOp2Result ok p1 k1 p2 k2


getOp1Result :: (SingI ok, SingI p1, SingI k1) => proxy ok -> D p1 k1 a -> Maybe (SomeOp1Result ok p1 k1)
getOp1Result op d =
  case singByProxy op of
    SOpNum ->
      case matchKind d of
        MKInt -> Just (SomeOp1Result Op1Num)
        MKReal -> Just (SomeOp1Result Op1Num)
        _ -> Nothing
    SOpLogical ->
      case matchKind d of
        MKLogical -> Just (SomeOp1Result Op1Logical)
        _ -> Nothing
    SOpProp ->
      case matchKind d of
        MKLogical -> Just (SomeOp1Result Op1Prop)
        _ -> Nothing
    _ -> Nothing


getOp2Result
  :: (SingI p1, SingI p2, SingI k1, SingI k2)
  => Sing ok -> D p1 k1 a -> D p2 k2 b -> Maybe (SomeOp2Result ok p1 k1 p2 k2)
getOp2Result op (d1 :: D p1 k1 a) (d2 :: D p2 k2 b) =
  case op of
    SOpNum ->
      withSingI (precMax (sing :: Sing p1) (sing :: Sing p2)) $
      case (matchKind d1, matchKind d2) of
        (MKInt, MKInt) -> Just (SomeOp2Result Op2Num)
        (MKInt, MKReal) -> Just (SomeOp2Result Op2Num)
        (MKReal, MKInt) -> Just (SomeOp2Result Op2Num)
        (MKReal, MKReal) -> Just (SomeOp2Result Op2Num)
        _ -> Nothing
    SOpEquality ->
      case (matchKind d1, matchKind d2) of
        (MKInt, MKInt) -> Just (SomeOp2Result Op2Eq)
        (MKInt, MKReal) -> Just (SomeOp2Result Op2Eq)
        (MKReal, MKInt) -> Just (SomeOp2Result Op2Eq)
        (MKReal, MKReal) -> Just (SomeOp2Result Op2Eq)
        (MKChar, MKChar) -> Just (SomeOp2Result Op2Eq)
        _ -> Nothing
    SOpRelational ->
      case (matchKind d1, matchKind d2) of
        (MKInt, MKInt) -> Just (SomeOp2Result Op2Rel)
        (MKInt, MKReal) -> Just (SomeOp2Result Op2Rel)
        (MKReal, MKInt) -> Just (SomeOp2Result Op2Rel)
        (MKReal, MKReal) -> Just (SomeOp2Result Op2Rel)
        (MKChar, MKChar) -> Just (SomeOp2Result Op2Rel)
        _ -> Nothing
    SOpLogical ->
      withSingI (precMax (sing :: Sing p1) (sing :: Sing p2)) $
      case (matchKind d1, matchKind d2) of
        (MKLogical, MKLogical) -> Just (SomeOp2Result Op2Logical)
        _ -> Nothing
    _ -> Nothing


getDFromOp1Result
  :: (SingI p2, SingI k2)
  => Op1Result ok p1 k1 p2 k2 -> Maybe (DWithParams p2 k2)
getDFromOp1Result _ = getDWithParams sing sing

getDFromOp2Result
  :: (SingI p3, SingI k3)
  => Op2Result ok p1 k1 p2 k2 p3 k3 -> Maybe (DWithParams p3 k3)
getDFromOp2Result _ = getDWithParams sing sing

--------------------------------------------------------------------------------
--  Extra instances
--------------------------------------------------------------------------------

showsDPrec :: Int -> D p k a -> a -> ShowS
showsDPrec p = \case
  DInt8 -> showsPrec p
  DInt16 -> showsPrec p
  DInt32 -> showsPrec p
  DInt64 -> showsPrec p
  DFloat -> showsPrec p
  DDouble -> showsPrec p
  DBool8 -> showsPrec p
  DBool16 -> showsPrec p
  DBool32 -> showsPrec p
  DBool64 -> showsPrec p
  DChar -> showsPrec p
  DProp -> showsPrec p

instance Pretty2 FortranOp where
  prettys2Prec p = \case
    OpLit d x -> showsDPrec p d x
    Op1 op _ _ _ x -> case op of
      OpNeg -> prettys1PrecUnop 9 "-" p x
      OpPos -> prettys1PrecUnop 9 "+" p x
      OpNot -> prettys1PrecUnop 9 "!" p x
      OpEmbedProp -> prettys1Prec p x
    Op2 op _ _ _ _ x y -> case op of
      OpAdd      -> prettys1PrecBinop 5 " + "    p x y
      OpSub      -> prettys1PrecBinop 5 " + "    p x y
      OpMul      -> prettys1PrecBinop 6 " + "    p x y
      OpDiv      -> prettys1PrecBinop 6 " + "    p x y
      OpEq       -> prettys1PrecBinop 4 " == "   p x y
      OpNE       -> prettys1PrecBinop 4 " /= "   p x y
      OpLT       -> prettys1PrecBinop 4 " < "    p x y
      OpLE       -> prettys1PrecBinop 4 " <= "   p x y
      OpGT       -> prettys1PrecBinop 4 " > "    p x y
      OpGE       -> prettys1PrecBinop 4 " >= "   p x y
      OpAnd      -> prettys1PrecBinop 3 " && "   p x y
      OpOr       -> prettys1PrecBinop 2 " || "   p x y
      OpEquiv    -> prettys1PrecBinop 1 " <=> "  p x y
      OpNotEquiv -> prettys1PrecBinop 1 " </=> " p x y

eqD :: D p1 k1 a -> D p2 k2 b -> a -> b -> Bool
eqD DInt8 DInt8 = (==)
eqD DInt16 DInt16 = (==)
eqD DInt32 DInt32 = (==)
eqD DInt64 DInt64 = (==)
eqD DFloat DFloat = (==)
eqD DDouble DDouble = (==)
eqD DBool8 DBool8 = (==)
eqD DBool16 DBool16 = (==)
eqD DBool32 DBool32 = (==)
eqD DBool64 DBool64 = (==)
eqD DChar DChar = (==)
eqD DProp DProp = (==)
eqD _ _ = \_ _ -> False

instance HEq FortranOp where
  liftHEq le eq x y = case (x, y) of
    (OpLit _ x1, OpLit _ x2) -> eq x1 x2
    (Op1 op1 _ d1 _ x1, Op1 op2 _ d2 _ x2) -> case (op1, op2) of
      (OpNeg, OpNeg) -> le (eqD d1 d2) x1 x2
      (OpPos, OpPos) -> le (eqD d1 d2) x1 x2
      (OpNot, OpNot) -> le (eqD d1 d2) x1 x2
      _ -> False
    (Op2 op1 _ dx1 dy1 _ x1 y1, Op2 op2 _ dx2 dy2 _ x2 y2) -> case (op1, op2) of
      (OpAdd     , OpAdd) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpSub     , OpSub) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpMul     , OpMul) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpDiv     , OpDiv) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpEq      , OpEq) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpNE      , OpNE) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpLT      , OpLT) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpLE      , OpLE) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpGT      , OpGT) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpGE      , OpGE) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpAnd     , OpAnd) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpOr      , OpOr) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpEquiv   , OpEquiv) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      (OpNotEquiv, OpNotEquiv) -> le (eqD dx1 dx2) x1 x2 && le (eqD dy1 dy2) y1 y2
      _ -> False
    _ -> False

instance (Eq1 t) => Eq1 (FortranOp t) where liftEq = liftLiftEq
instance (Eq1 t, Eq a) => Eq (FortranOp t a) where (==) = eq1
