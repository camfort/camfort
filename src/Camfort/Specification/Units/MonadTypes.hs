{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Camfort.Specification.Units.MonadTypes where

import           Camfort.Analysis
import           Camfort.Analysis.Annotations (Annotation)
import           Camfort.Specification.Units.Annotation (UA)
import           Camfort.Specification.Units.Environment (Constraints, PP, UnitInfo, VV)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Binary (Binary)
import           Data.Char (toLower)
import           Data.Data (Data)
import qualified Data.IntMap.Strict as IM
import           Data.List (find, isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import qualified Language.Fortran.AST as F

--------------------------------------------------------------------------------
--  Environment
--------------------------------------------------------------------------------

-- | Some options about how to handle literals.
data LiteralsOpt
  = LitPoly     -- ^ All literals are polymorphic.
  | LitUnitless -- ^ All literals are unitless.
  | LitMixed    -- ^ The literal "0" or "0.0" is fully parametric
                -- polymorphic. All other literals are monomorphic,
                -- possibly unitless.
  deriving (Show, Eq, Ord, Data)

instance Read LiteralsOpt where
  readsPrec _ s = case find ((`isPrefixOf` map toLower s) . fst) ms of
                    Just (str, con) -> [(con, drop (length str) s)]
                    Nothing         -> []
    where
      ms = [ ("poly", LitPoly), ("unitless", LitUnitless), ("mixed", LitMixed)
           , ("litpoly", LitPoly), ("litunitless", LitUnitless), ("litmixed", LitMixed) ]

-- | Options for the unit solver
data UnitOpts = UnitOpts
  { uoLiterals :: LiteralsOpt               -- ^ how to handle literals
  , uninitializeds :: Bool                   -- ^ whether to suggest for uninitialized variables
  }
  deriving (Show, Data, Eq, Ord)

data UnitEnv = UnitEnv
  { unitOpts     :: UnitOpts
  , unitProgramFile :: F.ProgramFile Annotation
  }

--------------------------------------------------------------------------------
--  State
--------------------------------------------------------------------------------

-- | Function/subroutine name -> associated, parametric polymorphic constraints
type TemplateMap = M.Map F.Name Constraints

-- | Things that can be exported from modules
data NameParamKey
  = NPKParam PP Int     -- ^ Function/subroutine name, position of parameter
  | NPKVariable VV      -- ^ variable
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary NameParamKey

-- | mapped to a list of units (to be multiplied together)
type NameParamMap = M.Map F.ProgramUnitName (M.Map NameParamKey [UnitInfo])

-- | Variable => unit
type VarUnitMap   = M.Map VV UnitInfo
-- | Set of variables given explicit unit annotations
type GivenVarSet  = S.Set F.Name
-- | Alias name => definition
type UnitAliasMap = M.Map String UnitInfo
-- | Map of CallId to CallId
type CallIdMap    = IM.IntMap Int

-- | Working state for the monad
data UnitState = UnitState
  { usProgramFile  :: F.ProgramFile UA
  , usVarUnitMap   :: VarUnitMap
  , usGivenVarSet  :: GivenVarSet
  , usUnitAliasMap :: UnitAliasMap
  , usTemplateMap  :: TemplateMap
  , usNameParamMap :: NameParamMap
  , usCallIdRemap  :: CallIdMap
    -- | Next number to returned by 'freshId'.
  , usNextUnique   :: Int
  , usConstraints  :: Constraints }
  deriving (Show, Data)

--------------------------------------------------------------------------------
--  Monads
--------------------------------------------------------------------------------

-- | Analysis with access to 'UnitEnv' information.
type UnitAnalysis = ReaderT UnitEnv (AnalysisT () () IO)

-- | UnitSolvers are analyses annotated with unit information.
type UnitSolver = StateT UnitState UnitAnalysis
