{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -Wall      #-}

module Language.Fortran.TypeModel.Eval.Primitives where

import           Data.Int                         (Int16, Int32, Int64, Int8)
import           Data.Word                        (Word8)

import           Control.Lens
import           Control.Monad.Reader             (MonadReader (..))

import qualified Data.SBV                         as SBV
import           Data.SBV.Dynamic                 (SVal)
import           Data.SBV.Internals               (SBV (..))

import           Language.Fortran.TypeModel.Types

--------------------------------------------------------------------------------
-- * Types

data IntRepr a = MachineInt | ArbitraryInt deriving (Eq, Ord, Show)
data RealRepr a = MachineFloat | ArbitraryReal deriving (Eq, Ord, Show)
data BoolRepr a = IntBool | BitBool deriving (Eq, Ord, Show)

data PrimSymRepr a =
  PrimSymRepr
  { _psrKind     :: !SBV.Kind
  , _psrLiteral  :: !(a -> SVal)
  , _psrSymbolic :: !(String -> SBV.Symbolic SVal)
  }

data PrimSymSpec =
  PrimSymSpec
  { _pssInt8Repr   :: !(IntRepr Int8)
  , _pssInt16Repr  :: !(IntRepr Int16)
  , _pssInt32Repr  :: !(IntRepr Int32)
  , _pssInt64Repr  :: !(IntRepr Int64)
  , _pssFloatRepr  :: !(RealRepr Float)
  , _pssDoubleRepr :: !(RealRepr Double)
  , _pssBool8Repr  :: !(BoolRepr Bool8)
  , _pssBool16Repr :: !(BoolRepr Bool16)
  , _pssBool32Repr :: !(BoolRepr Bool32)
  , _pssBool64Repr :: !(BoolRepr Bool64)
  }

makeLenses ''PrimSymRepr
makeLenses ''PrimSymSpec

--------------------------------------------------------------------------------
-- * Standard specs

pssPrecise :: PrimSymSpec
pssPrecise = PrimSymSpec
  { _pssInt8Repr = MachineInt
  , _pssInt16Repr = MachineInt
  , _pssInt32Repr = MachineInt
  , _pssInt64Repr = MachineInt
  , _pssFloatRepr = MachineFloat
  , _pssDoubleRepr = MachineFloat
  , _pssBool8Repr = IntBool
  , _pssBool16Repr = IntBool
  , _pssBool32Repr = IntBool
  , _pssBool64Repr = IntBool
  }

pssIdealized :: PrimSymSpec
pssIdealized = PrimSymSpec
  { _pssInt8Repr = ArbitraryInt
  , _pssInt16Repr = ArbitraryInt
  , _pssInt32Repr = ArbitraryInt
  , _pssInt64Repr = ArbitraryInt
  , _pssFloatRepr = ArbitraryReal
  , _pssDoubleRepr = ArbitraryReal
  , _pssBool8Repr = BitBool
  , _pssBool16Repr = BitBool
  , _pssBool32Repr = BitBool
  , _pssBool64Repr = BitBool
  }

pssWithArbitraryInts :: Bool -> PrimSymSpec -> PrimSymSpec
pssWithArbitraryInts useArbitrary
  | useArbitrary =
    set pssInt8Repr ArbitraryInt .
    set pssInt16Repr ArbitraryInt .
    set pssInt32Repr ArbitraryInt .
    set pssInt64Repr ArbitraryInt
  | otherwise =
    set pssInt8Repr MachineInt .
    set pssInt16Repr MachineInt .
    set pssInt32Repr MachineInt .
    set pssInt64Repr MachineInt

pssWithArbitraryReals :: Bool -> PrimSymSpec -> PrimSymSpec
pssWithArbitraryReals useArbitrary
  | useArbitrary =
    set pssFloatRepr ArbitraryReal .
    set pssDoubleRepr ArbitraryReal
  | otherwise =
    set pssFloatRepr MachineFloat .
    set pssDoubleRepr MachineFloat

--------------------------------------------------------------------------------
-- * Using specs

makeSymRepr :: PrimSymSpec -> Prim p k a -> PrimSymRepr a
makeSymRepr spec = \case
  PInt8   -> intRepr pssInt8Repr
  PInt16  -> intRepr pssInt16Repr
  PInt32  -> intRepr pssInt32Repr
  PInt64  -> intRepr pssInt64Repr
  PFloat  -> realRepr pssFloatRepr
  PDouble -> realRepr pssDoubleRepr
  PBool8  -> boolRepr getBool8 pssBool8Repr
  PBool16 -> boolRepr getBool16 pssBool16Repr
  PBool32 -> boolRepr getBool32 pssBool32Repr
  PBool64 -> boolRepr getBool64 pssBool64Repr
  PChar   -> bySymWord (0 :: Word8) getChar8
  where
    intRepr :: (Integral a, SBV.SymWord a) => Lens' PrimSymSpec (IntRepr a) -> PrimSymRepr a
    intRepr l = case spec ^. l of
      MachineInt   -> bySymWord 0 id
      ArbitraryInt -> bySymWord (0 :: Integer) fromIntegral

    realRepr :: (RealFloat a, SBV.SymWord a) => Lens' PrimSymSpec (RealRepr a) -> PrimSymRepr a
    realRepr l = case spec ^. l of
      MachineFloat  -> bySymWord 0 id
      ArbitraryReal -> bySymWord (0 :: SBV.AlgReal) realToFrac

    boolRepr :: (Integral b, SBV.SymWord b) => (a -> b) -> Lens' PrimSymSpec (BoolRepr a) -> PrimSymRepr a
    boolRepr unwrap l = case spec ^. l of
      IntBool -> bySymWord 0 unwrap
      BitBool -> bySymWord (False :: Bool) (toBool . unwrap)

    bySymWord :: (SBV.SymWord b) => b -> (a -> b) -> PrimSymRepr a
    bySymWord (repValue :: b) fromPrim =
      PrimSymRepr
      { _psrKind = SBV.kindOf repValue
      , _psrLiteral = unSBV . SBV.literal . fromPrim
      , _psrSymbolic = fmap (unSBV :: SBV b -> SVal) . SBV.symbolic
      }

    toBool :: (Ord a, Num a) => a -> Bool
    toBool x = x > 0

--------------------------------------------------------------------------------
-- * Monadic Accessors

class HasSymReprs r where
  getSymRepr :: r -> Prim p k a -> PrimSymRepr a

newtype SymReprEnv = SymReprEnv (forall p k a. Prim p k a -> PrimSymRepr a)

instance HasSymReprs SymReprEnv where
  getSymRepr (SymReprEnv k) = k

primSBVKind :: (MonadReader r m, HasSymReprs r) => Prim p k a -> m SBV.Kind
primSBVKind p = view (to (flip getSymRepr p) . psrKind)

primLit :: (MonadReader r m, HasSymReprs r) => Prim p k a -> a -> m SVal
primLit p a = do
  lit <- view (to (flip getSymRepr p) . psrLiteral)
  return (lit a)

primSymbolic :: (MonadReader r m, HasSymReprs r) => Prim p k a -> String -> m (SBV.Symbolic SVal)
primSymbolic p nm = do
  symbolic <- view (to (flip getSymRepr p) . psrSymbolic)
  return (symbolic nm)
