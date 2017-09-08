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

module Language.Fortran.Model.Vars
  ( -- * Names
    NamePair(..)
  , npSource
  , npUnique
  , SourceName(..)
  , UniqueName(..)
    -- * Variables
  , FortranVar(..)
  ) where

import           Data.Typeable                      ((:~:) (..))

import           Control.Lens                       hiding (Index, op)
import           Control.Monad.Reader               (MonadReader (..))

import           Data.SBV.Dynamic

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

instance Pretty SourceName where
  pretty = view _Wrapped

-- | The pretty version of a 'NamePair' is its source name.
instance Pretty NamePair where
  pretty = pretty . view npSource

--------------------------------------------------------------------------------
--  Fortran Variables
--------------------------------------------------------------------------------

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

arraySymbolic :: (MonadReader r m, HasPrimReprHandlers r) => Index i -> ArrValue a -> String -> m (Symbolic SArr)
arraySymbolic (Index ixPrim) (ArrValue valPrim) nm = do
  k1 <- primSBVKind ixPrim
  k2 <- primSBVKind valPrim
  return $ newSArr (k1, k2) (const nm)

-- pairProxy :: p1 a -> p2 b -> Proxy '(a, b)
-- pairProxy _ _ = Proxy
