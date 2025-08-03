{-# LANGUAGE CPP                   #-}
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

{-|

Defines a strongly-typed representation of Fortran variables.

-}
module Language.Fortran.Model.Vars
  (
    -- * Variables
    FortranVar(..)
    -- * Names
  , NamePair(..)
  , npSource
  , npUnique
  , SourceName(..)
  , UniqueName(..)
  ) where

import           Data.Typeable                      ((:~:) (..))

import           Control.Lens                       hiding (Index, op)

import           Data.SBV.Dynamic
import           Data.SBV                           (MonadSymbolic(..))

import           Control.Monad.Trans.Class          (lift)

import           Data.Vinyl                         (rtraverse)

import qualified Language.Fortran.AST               as F

import           Language.Expression.Pretty
import           Language.Verification

import           Language.Fortran.Model.Repr
import           Language.Fortran.Model.Repr.Prim
import           Language.Fortran.Model.Types
import           Language.Fortran.Model.Types.Match

--------------------------------------------------------------------------------
--  Names
--------------------------------------------------------------------------------

-- | Newtype wrapper for source-level variable names.
newtype SourceName = SourceName { getSourceName :: F.Name }
  deriving (Eq, Ord)

instance Show SourceName where show = show . getSourceName

-- | Newtype wrapper for unique variable names.
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

instance Pretty SourceName where
  pretty = view _Wrapped

-- | The pretty version of a 'NamePair' is its source name.
instance Pretty NamePair where
  pretty = pretty . view npSource

--------------------------------------------------------------------------------
--  Fortran Variables
--------------------------------------------------------------------------------

-- | A variable representing a value of type @a@. Contains a @'D' a@ as proof
-- that the type has a corresponding Fortran type, and the variable name.
data FortranVar a where
  FortranVar :: D a -> NamePair -> FortranVar a

instance VerifiableVar FortranVar where
  type VarKey FortranVar = UniqueName
  type VarSym FortranVar = CoreRepr
  type VarEnv FortranVar = PrimReprHandlers

  symForVar (FortranVar d np) env =
    case d of
      DPrim prim -> CRPrim d <$> primSymbolic prim uniqueName env
      DArray i val -> CRArray d <$> arraySymbolic i val uniqueName env
      DData _ _ -> fail "User-defined data type variables are not supported yet"
    where
      uniqueName = np ^. npUnique . _Wrapped

  varKey (FortranVar _ np) = np ^. npUnique

  eqVarTypes (FortranVar d1 _) (FortranVar d2 _) = eqD d1 d2

  castVarSym (FortranVar d1 _) s = case s of
    CRPrim d2 _  | Just Refl <- eqD d1 d2 -> Just s
    CRArray d2 _ | Just Refl <- eqD d1 d2 -> Just s
    CRData d2 _  | Just Refl <- eqD d1 d2 -> Just s

    -- Variables can't have the 'Bool' type so 'CRProp' can't be casted.
    _            -> Nothing

instance Pretty1 FortranVar where
  pretty1 (FortranVar _ np) = pretty np

--------------------------------------------------------------------------------
--  Internals
--------------------------------------------------------------------------------

arraySymbolic :: (HasPrimReprHandlers r) => Index i -> ArrValue a -> String -> r -> Symbolic (ArrRepr i a)
arraySymbolic ixIndex@(Index ixPrim) valAV nm env =
  case valAV of
    ArrPrim valPrim -> ARPrim <$> arraySymbolicPrim ixPrim valPrim nm env
    ArrData _ valData -> do
      valReprs <- rtraverse (traverseField' (\av -> arraySymbolic ixIndex av nm env)) valData
      return $ ARData valReprs

arraySymbolicPrim :: (HasPrimReprHandlers r) => Prim p1 k1 i -> Prim p2 k2 a -> String -> r -> Symbolic SArr
arraySymbolicPrim ixPrim valPrim nm env = do
  k1 <- lift . return $ primSBVKind ixPrim env
  k2 <- lift . return $ primSBVKind valPrim env
  state <- symbolicEnv
#if MIN_VERSION_sbv(10,0,0)
  lift $ newSArr state (k1, k2) (\i -> nm ++ "_" ++ show i) (Right "")
#else
  lift $ newSArr state (k1, k2) (\i -> nm ++ "_" ++ show i) Nothing
#endif
