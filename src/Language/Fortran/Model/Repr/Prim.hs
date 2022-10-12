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

{-|

= Handling primitive Fortran values symbolically.

There are a few challenges that this module attempts to solve:

Fortran uses fixed-width machine integers and floating point reals. Sometimes we
might want to reason about these directly (which is supported by SBV and
therefore feasible). However, sometimes they get in the way of the logic and we
just want to pretend that they're the pure mathematical values that they
approximate. For example floating point addition obeys very few algebraic laws,
so most theorems about real numbers don't hold at all for floating point
numbers.

In addition, Fortran's boolean values are actually arbitrary signed integers. If
we treat all boolean values symbolically as bit-vectors, logic can become very
slow; so it might be best to pretend that all booleans are single bits. However,
sometimes we might want to verify properties that rely on the actual bit-vector
representation of booleans.

This module deals with these problems by abstracting over the choices: the user
should be able to choose which representation they want to use for each
primitive data type.

The user provides a 'PrimReprSpec' which specifies how each data type should be
treated. Some examples are provided: 'prsPrecise' treats all values precisely as
they are represented in the Fortran program. This makes logic slow and makes it
very difficult to prove many things, but it is most accurate. On the other hand,
'prsIdealized' treats all values as their idealized mathematical equivalents.
This makes logic fast and lots intuitive properties can be proved easily.
However, these properties often will not hold in actual running Fortran
programs: sometimes in weird edge cases and sometimes in sensible-seeming
executions. It would be interesting future work to provide an analysis that
helps to determine which of the two applies to a particular program!

-}
module Language.Fortran.Model.Repr.Prim where

import           Data.Int                         (Int16, Int32, Int64, Int8)
import           Data.Word                        (Word8)

import           Control.Lens
import           Control.Monad.Reader             (MonadReader (..))

import qualified Data.SBV                         as SBV
import           Data.SBV.Dynamic                 (SVal)
import           Data.SBV.Internals               (SBV (..))

import           Language.Fortran.Model.Types

--------------------------------------------------------------------------------
-- * Types

data IntRepr a = MachineInt | ArbitraryInt deriving (Eq, Ord, Show)
data RealRepr a = MachineFloat | ArbitraryReal deriving (Eq, Ord, Show)
data BoolRepr a = IntBool | BitBool deriving (Eq, Ord, Show)

data PrimReprHandler a =
  PrimReprHandler
  { _prhKind     :: !SBV.Kind
  , _prhLiteral  :: !(a -> SVal)
  , _prhSymbolic :: !(String -> SBV.Symbolic SVal)
  }

data PrimReprSpec =
  PrimReprSpec
  { _prsInt8Repr   :: !(IntRepr Int8)
  , _prsInt16Repr  :: !(IntRepr Int16)
  , _prsInt32Repr  :: !(IntRepr Int32)
  , _prsInt64Repr  :: !(IntRepr Int64)
  , _prsFloatRepr  :: !(RealRepr Float)
  , _prsDoubleRepr :: !(RealRepr Double)
  , _prsBool8Repr  :: !(BoolRepr Bool8)
  , _prsBool16Repr :: !(BoolRepr Bool16)
  , _prsBool32Repr :: !(BoolRepr Bool32)
  , _prsBool64Repr :: !(BoolRepr Bool64)
  }

--------------------------------------------------------------------------------
-- ** Lenses

makeLenses ''PrimReprHandler
makeLenses ''PrimReprSpec

--------------------------------------------------------------------------------
-- * Standard specs

prsPrecise :: PrimReprSpec
prsPrecise = PrimReprSpec
  { _prsInt8Repr = MachineInt
  , _prsInt16Repr = MachineInt
  , _prsInt32Repr = MachineInt
  , _prsInt64Repr = MachineInt
  , _prsFloatRepr = MachineFloat
  , _prsDoubleRepr = MachineFloat
  , _prsBool8Repr = IntBool
  , _prsBool16Repr = IntBool
  , _prsBool32Repr = IntBool
  , _prsBool64Repr = IntBool
  }

prsIdealized :: PrimReprSpec
prsIdealized = PrimReprSpec
  { _prsInt8Repr = ArbitraryInt
  , _prsInt16Repr = ArbitraryInt
  , _prsInt32Repr = ArbitraryInt
  , _prsInt64Repr = ArbitraryInt
  , _prsFloatRepr = ArbitraryReal
  , _prsDoubleRepr = ArbitraryReal
  , _prsBool8Repr = BitBool
  , _prsBool16Repr = BitBool
  , _prsBool32Repr = BitBool
  , _prsBool64Repr = BitBool
  }

prsWithArbitraryInts :: Bool -> PrimReprSpec -> PrimReprSpec
prsWithArbitraryInts useArbitrary
  | useArbitrary =
    set prsInt8Repr ArbitraryInt .
    set prsInt16Repr ArbitraryInt .
    set prsInt32Repr ArbitraryInt .
    set prsInt64Repr ArbitraryInt
  | otherwise =
    set prsInt8Repr MachineInt .
    set prsInt16Repr MachineInt .
    set prsInt32Repr MachineInt .
    set prsInt64Repr MachineInt

prsWithArbitraryReals :: Bool -> PrimReprSpec -> PrimReprSpec
prsWithArbitraryReals useArbitrary
  | useArbitrary =
    set prsFloatRepr ArbitraryReal .
    set prsDoubleRepr ArbitraryReal
  | otherwise =
    set prsFloatRepr MachineFloat .
    set prsDoubleRepr MachineFloat

--------------------------------------------------------------------------------
-- * Using specs

makeSymRepr :: PrimReprSpec -> Prim p k a -> PrimReprHandler a
makeSymRepr spec = \case
  PInt8   -> intRepr prsInt8Repr
  PInt16  -> intRepr prsInt16Repr
  PInt32  -> intRepr prsInt32Repr
  PInt64  -> intRepr prsInt64Repr
  PFloat  -> realRepr prsFloatRepr
  PDouble -> realRepr prsDoubleRepr
  PBool8  -> boolRepr getBool8 prsBool8Repr
  PBool16 -> boolRepr getBool16 prsBool16Repr
  PBool32 -> boolRepr getBool32 prsBool32Repr
  PBool64 -> boolRepr getBool64 prsBool64Repr
  PChar   -> bySymWord (0 :: Word8) getChar8
  where
    intRepr
      :: (Integral a, SBV.SymVal a)
      => Lens' PrimReprSpec (IntRepr a) -> PrimReprHandler a
    intRepr l = case spec ^. l of
      MachineInt   -> bySymWord 0 id
      ArbitraryInt -> bySymWord (0 :: Integer) fromIntegral

    realRepr
      :: (RealFloat a, SBV.SymVal a)
      => Lens' PrimReprSpec (RealRepr a) -> PrimReprHandler a
    realRepr l = case spec ^. l of
      MachineFloat  -> bySymWord 0 id
      ArbitraryReal -> bySymWord (0 :: SBV.AlgReal) realToFrac

    boolRepr
      :: (Integral b, SBV.SymVal b)
      => (a -> b) -> Lens' PrimReprSpec (BoolRepr a) -> PrimReprHandler a
    boolRepr unwrap l = case spec ^. l of
      IntBool -> bySymWord 0 unwrap
      BitBool -> bySymWord (False :: Bool) (toBool . unwrap)

    bySymWord :: (SBV.SymVal b) => b -> (a -> b) -> PrimReprHandler a
    bySymWord (repValue :: b) fromPrim =
      PrimReprHandler
      { _prhKind = SBV.kindOf repValue
      , _prhLiteral = unSBV . SBV.literal . fromPrim
      , _prhSymbolic = fmap (unSBV :: SBV b -> SVal) . SBV.symbolic
      }

    toBool :: (Ord a, Num a) => a -> Bool
    toBool x = x > 0

--------------------------------------------------------------------------------
-- * Monadic Accessors

class HasPrimReprHandlers r where
  primReprHandlers :: r -> PrimReprHandlers
  primReprHandlers env = PrimReprHandlers (primReprHandler env)

  -- note that we must eta expand due to GHC 9.0 simplified subsumption
  primReprHandler :: r -> Prim p k a -> PrimReprHandler a
  primReprHandler r p = unPrimReprHandlers (primReprHandlers r) p

newtype PrimReprHandlers =
  PrimReprHandlers { unPrimReprHandlers :: forall p k a. Prim p k a -> PrimReprHandler a }

instance HasPrimReprHandlers PrimReprHandlers where
  primReprHandlers = id

primSBVKind :: (MonadReader r m, HasPrimReprHandlers r) => Prim p k a -> m SBV.Kind
primSBVKind p = view (to (flip primReprHandler p) . prhKind)

primLit :: (MonadReader r m, HasPrimReprHandlers r) => Prim p k a -> a -> m SVal
primLit p a = do
  lit <- view (to (flip primReprHandler p) . prhLiteral)
  return (lit a)

primSymbolic
  :: (MonadReader r m, HasPrimReprHandlers r)
  => Prim p k a -> String -> m (SBV.Symbolic SVal)
primSymbolic p nm = do
  symbolic <- view (to (flip primReprHandler p) . prhSymbolic)
  return (symbolic nm)
