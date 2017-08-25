{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

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
  ) where

import           Data.Int                         (Int16, Int32, Int64, Int8)
import           Data.Typeable                    ((:~:) (..), Proxy (..))
import           Data.Word                        (Word8)

import           Control.Lens                     hiding (Index, op)

import           Data.SBV                         (SymWord (..), newArray)
import           Data.SBV.Dynamic
import           Data.SBV.Internals               (SArray (..), SBV (..))

import qualified Language.Fortran.AST             as F

import           Language.Expression.Pretty
import           Language.Verification

import           Language.Fortran.TypeModel.Match
import           Language.Fortran.TypeModel.Types
import           Language.Fortran.TypeModel.Operator.Eval

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
      DArray ix val -> SRArray d <$> varForArray uniqueName ix val
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
--  Internals
--------------------------------------------------------------------------------

varForPrim :: String -> Prim p k a -> Symbolic SVal
varForPrim = flip primSymbolic

varForArray :: String -> Index i -> Prim p k a -> Symbolic SArr
varForArray nm (Index ixPrim) valPrim =
  let k1 = primSBVKind ixPrim
      k2 = primSBVKind valPrim
  in newSArr (k1, k2) (const nm)
