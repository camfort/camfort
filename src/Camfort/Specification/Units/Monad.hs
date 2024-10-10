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
  ( UA, VV, UnitSolver, UnitOpts(..), unitOpts0, UnitState, UnitEnv(..), LiteralsOpt(..)
  , VarUnitMap, GivenVarSet, UnitAliasMap, TemplateMap, CallIdMap
  , NameParamMap, NameParamKey(..)
    -- ** State Helpers
  , freshId
    -- *** Getters
  , getConstraints
  , getNameParamMap
  , getProgramFile
  , getTemplateMap
  , getUnitAliasMap
  , getVarUnitMap
  , usCallIdRemap
  , usConstraints
  , usGivenVarSet
  , usNameParamMap
  , usProgramFile
  , usTemplateMap
  , usUnitAliasMap
  , usVarUnitMap
    -- *** Modifiers
  , modifyCallIdRemap
  , modifyCallIdRemapM
  , modifyConstraints
  , modifyGivenVarSet
  , modifyNameParamMap
  , modifyProgramFile
  , modifyProgramFileM
  , modifyTemplateMap
  , modifyUnitAliasMap
  , modifyVarUnitMap
    -- ** Runners
  , runUnitSolver
  , runUnitAnalysis
  ) where

import           Camfort.Analysis
import           Camfort.Specification.Units.Annotation (UA)
import           Camfort.Specification.Units.Environment (Constraints, VV)
import           Camfort.Specification.Units.MonadTypes
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Language.Fortran.AST as F

unitOpts0 :: UnitOpts
unitOpts0 = UnitOpts LitMixed False

--------------------------------------------------

unitState0 :: F.ProgramFile UA -> UnitState
unitState0 pf = UnitState { usProgramFile  = pf
                          , usVarUnitMap   = mempty
                          , usGivenVarSet  = mempty
                          , usUnitAliasMap = mempty
                          , usTemplateMap  = mempty
                          , usNameParamMap = mempty
                          , usNextUnique   = 0
                          , usCallIdRemap  = mempty
                          , usConstraints  = [] }

-- helper functions

-- | Generate a number guaranteed to be unique in the current analysis.
freshId :: UnitSolver Int
freshId = do
  s <- get
  let i = usNextUnique s
  put $ s { usNextUnique = i + 1 }
  pure i

getConstraints :: UnitSolver Constraints
getConstraints = gets usConstraints

getNameParamMap :: UnitSolver NameParamMap
getNameParamMap = gets usNameParamMap

getProgramFile :: UnitSolver (F.ProgramFile UA)
getProgramFile = gets usProgramFile

getTemplateMap :: UnitSolver TemplateMap
getTemplateMap = gets usTemplateMap

getUnitAliasMap :: UnitSolver UnitAliasMap
getUnitAliasMap = gets usUnitAliasMap

getVarUnitMap :: UnitSolver VarUnitMap
getVarUnitMap = gets usVarUnitMap

modifyVarUnitMap :: (VarUnitMap -> VarUnitMap) -> UnitSolver ()
modifyVarUnitMap f = modify (\ s -> s { usVarUnitMap = f (usVarUnitMap s) })

modifyCallIdRemap :: (CallIdMap -> CallIdMap) -> UnitSolver ()
modifyCallIdRemap f = modify (\ s -> s { usCallIdRemap = f (usCallIdRemap s) })

modifyConstraints :: (Constraints -> Constraints) -> UnitSolver ()
modifyConstraints f = modify (\ s -> s { usConstraints = f (usConstraints s) })

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
  modifyCallIdRemap (const idMap')
  return x

--------------------------------------------------

-- | Runs the stateful part of the unit solver.
runUnitSolver :: F.ProgramFile UA -> UnitSolver a -> UnitAnalysis (a, UnitState)
runUnitSolver pf solver = do
  runStateT solver (unitState0 pf)

runUnitAnalysis :: UnitEnv -> UnitAnalysis a -> AnalysisT () () IO a
runUnitAnalysis env = flip runReaderT env
