{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Defines the monad for the units-of-measure modules -}
module Camfort.Specification.Units.Monad
  ( UA, VV, UnitSolver, UnitOpts(..), unitOpts0, UnitLogs, UnitState(..), LiteralsOpt(..), UnitException
  , whenDebug, modifyVarUnitMap, modifyGivenVarSet, modifyUnitAliasMap
  , VarUnitMap, GivenVarSet, UnitAliasMap, TemplateMap, CallIdMap
  , modifyTemplateMap, modifyNameParamMap, modifyProgramFile, modifyProgramFileM, modifyCallIdRemapM
  , runUnitSolver, evalUnitSolver, execUnitSolver
  , CompiledUnits(..), NameParamMap, NameParamKey(..), emptyCompiledUnits )
where

import Control.Monad.RWS.Strict
import Control.Monad.Trans.Except
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Data.Char (toLower)
import Data.Data (Data)
import Data.List (find, isPrefixOf)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.AST as F
import Language.Fortran.Util.ModFile
import Camfort.Specification.Units.Environment (UnitInfo, Constraints(..), VV, PP)
import Camfort.Analysis.Annotations (UA)


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
  { uoDebug          :: Bool                      -- ^ debugging mode?
  , uoLiterals       :: LiteralsOpt               -- ^ how to handle literals
  , uoModFiles       :: M.Map String ModFile      -- ^ map of included modules
  }
  deriving (Show, Data, Eq, Ord)

unitOpts0 :: UnitOpts
unitOpts0 = UnitOpts False LitMixed M.empty

-- | Function/subroutine name -> associated, parametric polymorphic constraints
type TemplateMap = M.Map F.Name Constraints

-- | Things that can be exported from modules
data NameParamKey
  = NPKParam PP Int     -- ^ Function/subroutine name, position of parameter
  | NPKVariable VV      -- ^ variable
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary NameParamKey

-- | mapped to a list of units (to be multiplied together)
type NameParamMap = M.Map NameParamKey [UnitInfo]

-- | The data-structure stored in 'fortran-src mod files'
data CompiledUnits = CompiledUnits { cuTemplateMap  :: TemplateMap
                                   , cuNameParamMap :: NameParamMap }
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary CompiledUnits

emptyCompiledUnits :: CompiledUnits
emptyCompiledUnits = CompiledUnits M.empty M.empty

--------------------------------------------------

-- | The monad
type UnitSolver a = ExceptT UnitException (RWS UnitOpts UnitLogs UnitState) a

--------------------------------------------------

-- Not in use, but might be useful someday.
type UnitException = ()

--------------------------------------------------

-- Read-only options for the unit solver.

-- | Only run the argument if debugging mode enabled.
whenDebug :: UnitSolver () -> UnitSolver ()
whenDebug m = fmap uoDebug ask >>= \ d -> when d m

--------------------------------------------------

-- Track some logging information in the monad.
type UnitLogs = String

--------------------------------------------------

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
  , usLitNums      :: Int
  , usCallIds      :: Int
  , usCallIdRemap  :: CallIdMap
  , usConstraints  :: Constraints }
  deriving (Show, Data)

unitState0 pf = UnitState { usProgramFile  = pf
                          , usVarUnitMap   = M.empty
                          , usGivenVarSet  = S.empty
                          , usUnitAliasMap = M.empty
                          , usTemplateMap  = M.empty
                          , usNameParamMap = M.empty
                          , usLitNums      = 0
                          , usCallIds      = 0
                          , usCallIdRemap  = IM.empty
                          , usConstraints  = [] }

-- helper functions
modifyVarUnitMap :: (VarUnitMap -> VarUnitMap) -> UnitSolver ()
modifyVarUnitMap f = modify (\ s -> s { usVarUnitMap = f (usVarUnitMap s) })

modifyGivenVarSet :: (GivenVarSet -> GivenVarSet) -> UnitSolver ()
modifyGivenVarSet f = modify (\ s -> s { usGivenVarSet = f (usGivenVarSet s) })

modifyUnitAliasMap :: (UnitAliasMap -> UnitAliasMap) -> UnitSolver ()
modifyUnitAliasMap f = modify (\ s -> s { usUnitAliasMap = f (usUnitAliasMap s) })

modifyTemplateMap :: (TemplateMap -> TemplateMap) -> UnitSolver ()
modifyTemplateMap f = modify (\ s -> s { usTemplateMap = f (usTemplateMap s) })

modifyNameParamMap :: (NameParamMap -> NameParamMap) -> UnitSolver ()
modifyNameParamMap f = modify (\ s -> s { usNameParamMap = f (usNameParamMap s) })

modifyProgramFile :: (F.ProgramFile UA -> F.ProgramFile UA) -> UnitSolver ()
modifyProgramFile f = modify (\ s -> s { usProgramFile = f (usProgramFile s) })

modifyProgramFileM :: (F.ProgramFile UA -> UnitSolver (F.ProgramFile UA)) -> UnitSolver ()
modifyProgramFileM f = do
  pf <- fmap usProgramFile get
  pf' <- f pf
  modify (\ s -> s { usProgramFile = pf' })

modifyCallIdRemapM :: (CallIdMap -> UnitSolver (a, CallIdMap)) -> UnitSolver a
modifyCallIdRemapM f = do
  idMap <- gets usCallIdRemap
  (x, idMap') <- f idMap
  modify (\ s -> s { usCallIdRemap = idMap' })
  return x

--------------------------------------------------

-- | Run the unit solver monad.
runUnitSolver :: UnitOpts -> F.ProgramFile UA -> UnitSolver a -> (Either UnitException a, UnitState, UnitLogs)
runUnitSolver o pf m = runRWS (runExceptT m) o (unitState0 pf)

evalUnitSolver :: UnitOpts -> F.ProgramFile UA -> UnitSolver a -> (Either UnitException a, UnitLogs)
evalUnitSolver o pf m = (ea, l) where (ea, _, l) = runUnitSolver o pf m

execUnitSolver :: UnitOpts -> F.ProgramFile UA -> UnitSolver a -> Either UnitException (UnitState, UnitLogs)
execUnitSolver o pf m = case runUnitSolver o pf m of
  (Left e, _, _)  -> Left e
  (Right _, s, l) -> Right (s, l)
