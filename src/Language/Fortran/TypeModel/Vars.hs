{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wall #-}

-- TODO: Variables for user-defined data types

module Language.Fortran.TypeModel.Vars
  ( -- * Names
    NamePair(..)
  , npSource
  , npUnique
  , SourceName(..)
  , UniqueName(..)
    -- * Variables
  , FortranVar(..)
    -- * Variable updates
  , VarUpdate(..)
  , applyVarUpdate
  ) where

import           Data.Typeable                            ((:~:) (..),
                                                           Proxy (..))

import           Control.Lens                             hiding (Index, op)

import           Data.SBV.Dynamic

import           Data.Singletons.TypeLits
import           Data.Vinyl.Lens

import qualified Language.Fortran.AST                     as F

import           Language.Expression
import           Language.Expression.Pretty
import           Language.Verification

import           Language.Fortran.TypeModel.Match
import           Language.Fortran.TypeModel.Operator.Eval
import           Language.Fortran.TypeModel.Types

--------------------------------------------------------------------------------
--  Names
--------------------------------------------------------------------------------

newtype SourceName = SourceName { getSourceName :: F.Name }
  deriving (Eq, Ord)

instance Show SourceName where show = show . getSourceName

newtype UniqueName = UniqueName { getUniqueName :: F.Name }
  deriving (Eq, Ord)

instance Show UniqueName where show = show . getUniqueName

makeWrapped ''SourceName
makeWrapped ''UniqueName

-- | A 'NamePair' represents the name of some part of a Fortran program,
-- including the human-readable source name and the unique name.
data NamePair =
  NamePair
  { _npUnique :: UniqueName
  , _npSource :: SourceName
  }
  deriving (Eq, Ord, Show)

makeLenses ''NamePair

-- | The pretty version of a 'NamePair' is its source name.
instance Pretty NamePair where
  pretty = view (npSource . _Wrapped)

--------------------------------------------------------------------------------
--  Fortran Variables
--------------------------------------------------------------------------------

data FortranVar a where
  FortranVar :: D a -> NamePair -> FortranVar a

instance VerifiableVar FortranVar where
  type VarKey FortranVar = UniqueName
  type VarSym FortranVar = SymRepr

  symForVar (FortranVar d np) =
    case d of
      DPrim prim -> SRPrim d <$> varForPrim uniqueName prim
      DArray i val -> SRArray d <$> varForArray uniqueName i val
      DData _ _ -> fail "User-defined data type variables are not supported yet"
    where
      uniqueName = np ^. npUnique . _Wrapped

  varKey (FortranVar _ np) = np ^. npUnique

  eqVarTypes (FortranVar d1 _) (FortranVar d2 _) = eqD d1 d2

  castVarSym (FortranVar d1 _) s = case s of
    SRPrim d2 _  | Just Refl <- eqD d1 d2 -> Just s
    SRArray d2 _ | Just Refl <- eqD d1 d2 -> Just s
    SRData d2 _  | Just Refl <- eqD d1 d2 -> Just s

    -- Variables can't have the 'Bool' type so 'SRProp' can't be casted.
    _            -> Nothing

instance Pretty1 FortranVar where
  pretty1 (FortranVar _ np) = pretty np

--------------------------------------------------------------------------------
--  Updating variables
--------------------------------------------------------------------------------

data VarUpdate t a where
  UpdatePrim
    :: t (PrimS a) -- ^ New value
    -> VarUpdate t (PrimS a)

  UpdateArray
    :: Index i
    -> ArrValue a
    -> t i -- ^ Index to update at
    -> t a -- ^ New values at that index
    -> VarUpdate t (Array i a)

  UpdateData
    :: RElem '(fieldName, a) fields i
    => SSymbol fieldName -- ^ Field to update
    -> VarUpdate t a     -- ^ Update to apply to that field
    -> VarUpdate t (Record recordName fields)

-- | Who'd have thought?
instance Operator VarUpdate where
  htraverseOp f = \case
    UpdatePrim x -> UpdatePrim <$> f x
    UpdateArray i v x y -> UpdateArray i v <$> f x <*> f y
    UpdateData s x -> UpdateData s <$> htraverseOp f x

applyVarUpdate :: VarUpdate SymRepr a -> SymRepr a -> SymRepr a
applyVarUpdate = \case
  -- For primitives, just replace old value with new
  UpdatePrim _ -> id

  -- For arrays, replace the old array with one updated at the particular index.
  UpdateArray (Index _) (ArrValue _) (SRPrim _ ixVal) (SRPrim _ aVal) -> \case
    SRArray arrD arr -> SRArray arrD (writeSArr arr ixVal aVal)

  UpdateData sFieldName valUpdate -> \case
    SRData dRecord record ->
      let fieldProxy = pairProxy sFieldName valUpdate
          update = overFieldRepr (applyVarUpdate valUpdate)
      in SRData dRecord (record & rlens fieldProxy %~ update)

--------------------------------------------------------------------------------
--  Internals
--------------------------------------------------------------------------------

varForPrim :: String -> Prim p k a -> Symbolic SVal
varForPrim = flip primSymbolic

varForArray :: String -> Index i -> ArrValue a -> Symbolic SArr
varForArray nm (Index ixPrim) (ArrValue valPrim) =
  let k1 = primSBVKind ixPrim
      k2 = primSBVKind valPrim
  in newSArr (k1, k2) (const nm)

pairProxy :: p1 a -> p2 b -> Proxy '(a, b)
pairProxy _ _ = Proxy
