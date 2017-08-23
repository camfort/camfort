{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}

-- TODO: Complex Numbers

module Language.Fortran.TypeModel.Types where

import           Data.Int                              (Int16, Int32, Int64,
                                                        Int8)
import           Data.Typeable
import           Data.Word                             (Word8)

import           Data.Singletons
import           Data.Singletons.TypeLits
import           GHC.TypeLits

import           Data.SBV.Dynamic                      hiding (KReal)

import           Data.Vinyl

import           Language.Fortran.TypeModel.Singletons

--------------------------------------------------------------------------------
--  Semantic Types
--------------------------------------------------------------------------------

newtype Bool8  = Bool8 { getBool8 :: Int8 } deriving (Show, Num, Eq)
newtype Bool16 = Bool16 { getBool16 :: Int16 } deriving (Show, Num, Eq)
newtype Bool32 = Bool32 { getBool32 :: Int32 } deriving (Show, Num, Eq)
newtype Bool64 = Bool64 { getBool64 :: Int64 } deriving (Show, Num, Eq)
newtype Char8  = Char8 { getChar8 :: Word8 } deriving (Show, Num, Eq)

--------------------------------------------------------------------------------
--  Primitive Types
--------------------------------------------------------------------------------

newtype PrimS a = PrimS a
  deriving (Show, Eq)

-- | Lists the allowed primitive Fortran types, with corresponding constraints
-- on precision, kind and semantic Haskell type.
data Prim p k a where
  PInt8          :: Prim 'P8   'KInt     (PrimS Int8)
  PInt16         :: Prim 'P16  'KInt     (PrimS Int16)
  PInt32         :: Prim 'P32  'KInt     (PrimS Int32)
  PInt64         :: Prim 'P64  'KInt     (PrimS Int64)

  PBool8         :: Prim 'P8   'KLogical (PrimS Bool8)
  PBool16        :: Prim 'P16  'KLogical (PrimS Bool16)
  PBool32        :: Prim 'P32  'KLogical (PrimS Bool32)
  PBool64        :: Prim 'P64  'KLogical (PrimS Bool64)

  PFloat         :: Prim 'P32  'KReal    (PrimS Float)
  PDouble        :: Prim 'P64  'KReal    (PrimS Double)

  PChar          :: Prim 'P8   'KChar    (PrimS Char8)

  -- PProp          :: Prim 'P64  'KProp    Bool

primS :: Prim p k a -> (forall a'. a ~ PrimS a' => Prim p k (PrimS a') -> r) -> r
primS p f = case p of
  PInt8   -> f p
  PInt16  -> f p
  PInt32  -> f p
  PInt64  -> f p
  PBool8  -> f p
  PBool16 -> f p
  PBool32 -> f p
  PBool64 -> f p
  PFloat  -> f p
  PDouble -> f p
  PChar   -> f p

--------------------------------------------------------------------------------
--  Arrays
--------------------------------------------------------------------------------

data Index a where
  Index :: Prim p 'KInt a -> Index a

newtype Array i a = Array [a]

--------------------------------------------------------------------------------
--  Records
--------------------------------------------------------------------------------

data RField field where
  RField :: SSymbol name -> D a -> RField '(name, a)

data Record name fields where
  Record :: SSymbol name -> Rec ElField fields -> Record name fields

--------------------------------------------------------------------------------
--  Fortran Types
--------------------------------------------------------------------------------

-- TODO: Support arrays of non-primitive values

data D a where
  DPrim :: Prim p k (PrimS a) -> D (PrimS a)
  DArray :: Index i -> Prim p k a -> D (Array i a)
  DData :: SSymbol name -> Rec RField fs -> D (Record name fs)

--------------------------------------------------------------------------------
--  SBV Representations
--------------------------------------------------------------------------------

data FieldRepr (field :: (Symbol, *)) where
  FR :: SSymbol name -> SymRepr a -> FieldRepr '(name, a)

data SymRepr a where
  SRPrim
    :: D (PrimS a)
    -> SVal
    -> SymRepr (PrimS a)

  SRArray
    :: D (Array i a)
    -> SArr
    -> SymRepr (Array i a)

  SRData
    :: D (Record name fs)
    -> Rec FieldRepr fs
    -> SymRepr (Record name fs)

-- --------------------------------------------------------------------------------
-- --  Dynamic Types
-- --------------------------------------------------------------------------------

-- data DynType
--   = DTInt
--   | DTFloat
--   | DTBool
--   | DTRec String [(String, DynType)]
--   | DTArr DynType DynType

-- --------------------------------------------------------------------------------
-- --  Conversion to static types
-- --------------------------------------------------------------------------------

-- data Some (f :: k) where
--   Some :: f a -> Some f

-- dynToD :: DynType -> Maybe (Some D)
-- dynToD = \case
--   DTInt -> Just $ Some $ DPrim PInt64
--   DTFloat -> Just $ Some $ DPrim PFloat
--   DTBool -> Just $ Some $ DPrim PBool8

--   DTRec recName fields -> do
--     SomeSymbol np <- return $ someSymbolVal recName
--     Some fs <- listToRecord dynToField fields

--     return $ Some $ DData (singByProxy np) fs

--   DTArr indexTy valTy -> do
--     Some i <- dynToIndex indexTy
--     Some v <- dynToD valTy
--     Just $ Some $ DArray i v

-- dynToIndex :: DynType -> Maybe (Some Index)
-- dynToIndex = \case
--   DTInt -> Just $ Some $ Index PInt64
--   _ -> Nothing

-- dynToField :: (String, DynType) -> Maybe (Some RField)
-- dynToField (fieldName, fieldTy) = do
--   SomeSymbol np <- return $ someSymbolVal fieldName
--   Some ty <- dynToD fieldTy
--   return $ Some $ (RField (singByProxy np) ty)


-- listToRecord :: Applicative t => (a -> t (Some f)) -> [a] -> t (Some (Rec f))
-- listToRecord f [] = pure (Some RNil)
-- listToRecord (f :: a -> t (Some f)) (x : xs) = cons <$> f x <*> listToRecord f xs
--   where
--     cons :: Some f -> Some (Rec f) -> Some (Rec f)
--     cons y ys = case (y, ys) of (Some z, Some zs) -> Some (z :& zs)
