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
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}


{- | Defines the monad for the units-of-measure modules -}
module Camfort.Specification.Units.Monad
  ( UnitSolver, UnitOpts(..), UnitLogs, UnitState(..), LiteralsOpt(..)
  , modifyVarUnitMap, modifyUnitAliasMap
  , runUnitSolver, evalUnitSolver, execUnitSolver ) where

import Control.Monad.RWS.Strict
import Data.Data
import qualified Data.Map as M
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.AST as F
import Camfort.Specification.Units.Environment (UnitInfo)

-- | The monad
type UnitSolver a = RWS UnitOpts UnitLogs UnitState a

--------------------------------------------------

data LiteralsOpt = LitPoly | LitUnitless | LitMixed deriving (Show, Read, Eq, Ord, Data)

data UnitOpts = UnitOpts
  { uoCriticals      :: Bool
  , uoDebug          :: Bool
  , uoLiterals       :: LiteralsOpt
  , uoNameMap        :: FAR.NameMap
  , uoArgumentDecls  :: Bool }
  deriving (Show, Read, Data, Eq, Ord)

--------------------------------------------------

type UnitLogs = String

--------------------------------------------------

type VarUnitMap = M.Map F.Name UnitInfo
type UnitAliasMap = M.Map String UnitInfo
data UnitState = UnitState
  { usVarUnitMap   :: VarUnitMap
  , usUnitAliasMap :: UnitAliasMap }
  deriving (Show, Data, Eq, Ord)

unitState0 = UnitState { usVarUnitMap   = M.empty
                       , usUnitAliasMap = M.empty }

modifyVarUnitMap :: (VarUnitMap -> VarUnitMap) -> UnitSolver ()
modifyVarUnitMap f = modify (\ s -> s { usVarUnitMap = f (usVarUnitMap s) })

modifyUnitAliasMap :: (UnitAliasMap -> UnitAliasMap) -> UnitSolver ()
modifyUnitAliasMap f = modify (\ s -> s { usUnitAliasMap = f (usUnitAliasMap s) })

--------------------------------------------------

runUnitSolver :: UnitOpts -> UnitSolver a -> (a, UnitState, UnitLogs)
runUnitSolver o m = runRWS m o unitState0
evalUnitSolver :: UnitOpts -> UnitSolver a -> (a, UnitLogs)
evalUnitSolver o m = evalRWS m o unitState0
execUnitSolver :: UnitOpts -> UnitSolver a -> (UnitState, UnitLogs)
execUnitSolver o m = execRWS m o unitState0
