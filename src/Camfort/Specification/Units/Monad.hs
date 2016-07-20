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
{-# LANGUAGE ScopedTypeVariables #-}

{- | Defines the monad for the units-of-measure modules -}
module Camfort.Specification.Units.Monad
  ( UA, UnitSolver, UnitOpts(..), UnitLogs, UnitState(..), LiteralsOpt(..), UnitException(..)
  , whenDebug, modifyVarUnitMap, modifyGivenVarSet, modifyUnitAliasMap
  , modifyTemplateMap, modifyProgramFile, modifyProgramFileM
  , runUnitSolver, evalUnitSolver, execUnitSolver )
where

import Control.Monad.RWS.Strict
import Control.Monad.Trans.Except
import Data.Data (Data)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.AST as F
import Camfort.Specification.Units.Environment (UnitInfo, UnitAnnotation, Constraints(..))
import Camfort.Analysis.Annotations (Annotation, A, UA)

--------------------------------------------------

-- | The monad
type UnitSolver a = ExceptT UnitException (RWS UnitOpts UnitLogs UnitState) a

--------------------------------------------------

data UnitException = UEIncompatible UnitInfo UnitInfo
  deriving (Show, Data, Eq, Ord)

--------------------------------------------------

data LiteralsOpt = LitPoly | LitUnitless | LitMixed deriving (Show, Read, Eq, Ord, Data)

data UnitOpts = UnitOpts
  { uoDebug          :: Bool
  , uoLiterals       :: LiteralsOpt
  , uoNameMap        :: FAR.NameMap
  , uoArgumentDecls  :: Bool }
  deriving (Show, Read, Data, Eq, Ord)

-- | Only run the argument if debugging mode enabled.
whenDebug :: UnitSolver () -> UnitSolver ()
whenDebug m = fmap uoDebug ask >>= \ d -> when d m

--------------------------------------------------

type UnitLogs = String

--------------------------------------------------

type VarUnitMap   = M.Map F.Name UnitInfo
type GivenVarSet  = S.Set F.Name
type UnitAliasMap = M.Map String UnitInfo
type TemplateMap  = M.Map F.Name Constraints

data UnitState = UnitState
  { usProgramFile  :: F.ProgramFile UA
  , usVarUnitMap   :: VarUnitMap
  , usGivenVarSet  :: GivenVarSet
  , usUnitAliasMap :: UnitAliasMap
  , usTemplateMap  :: TemplateMap
  , usLitNums      :: Int
  , usConstraints  :: Constraints }
  deriving (Show, Data)

unitState0 pf = UnitState { usProgramFile  = pf
                          , usVarUnitMap   = M.empty
                          , usGivenVarSet  = S.empty
                          , usUnitAliasMap = M.empty
                          , usTemplateMap  = M.empty
                          , usLitNums      = 0
                          , usConstraints  = [] }

modifyVarUnitMap :: (VarUnitMap -> VarUnitMap) -> UnitSolver ()
modifyVarUnitMap f = modify (\ s -> s { usVarUnitMap = f (usVarUnitMap s) })

modifyGivenVarSet :: (GivenVarSet -> GivenVarSet) -> UnitSolver ()
modifyGivenVarSet f = modify (\ s -> s { usGivenVarSet = f (usGivenVarSet s) })

modifyUnitAliasMap :: (UnitAliasMap -> UnitAliasMap) -> UnitSolver ()
modifyUnitAliasMap f = modify (\ s -> s { usUnitAliasMap = f (usUnitAliasMap s) })

modifyTemplateMap :: (TemplateMap -> TemplateMap) -> UnitSolver ()
modifyTemplateMap f = modify (\ s -> s { usTemplateMap = f (usTemplateMap s) })

modifyProgramFile :: (F.ProgramFile UA -> F.ProgramFile UA) -> UnitSolver ()
modifyProgramFile f = modify (\ s -> s { usProgramFile = f (usProgramFile s) })

modifyProgramFileM :: (F.ProgramFile UA -> UnitSolver (F.ProgramFile UA)) -> UnitSolver ()
modifyProgramFileM f = do
  pf <- fmap usProgramFile get
  pf' <- f pf
  modify (\ s -> s { usProgramFile = pf' })

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
