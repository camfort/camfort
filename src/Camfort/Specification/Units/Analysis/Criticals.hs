{- |
Module      :  Camfort.Specification.Units.Analysis.Criticals
Description :  Critical-units analysis.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental

This module defines an analysis for finding the 'critical' variables in a program.
These critical variables form a set of variables that, when given unit annotations,
can be used to infer the unit types of all other variables in the program.
-}

module Camfort.Specification.Units.Analysis.Criticals
  ( inferCriticalVariables
  ) where

import           Control.Monad.State (get)
import qualified Data.Array            as A
import           Data.Generics.Uniplate.Operations
import           Data.List ((\\), findIndex)
import qualified Data.Map.Strict       as M
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Numeric.LinearAlgebra as H

import Camfort.Analysis.Annotations
import Camfort.Analysis.Fortran
  (analysisInput, writeDebug)
import Camfort.Specification.Units.InferenceBackend
  (constraintsToMatrix, rref)

-- Provides the types and data accessors used in this module
import           Camfort.Specification.Units.Analysis (UnitsAnalysis)
import qualified Camfort.Specification.Units.Annotation as UA
import           Camfort.Specification.Units.Environment
import           Camfort.Specification.Units.InferenceFrontend (runInference)
import           Camfort.Specification.Units.Monad

import qualified Language.Fortran.AST               as F
import qualified Language.Fortran.Analysis          as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import           Language.Fortran.Util.ModFile
import qualified Language.Fortran.Util.Position     as FU

-- | An inference of variables that must be provided with
-- unit annotations before units for all variables can be
-- resolved.
data Criticals = Criticals
  {
    -- | 'ProgramFile' analysis was performed upon.
    criticalsPf           :: F.ProgramFile Annotation
    -- | The inferred critical variables.
  , criticalsVariables    :: [UnitInfo]
    -- | Map of all declarations.
  , criticalsDeclarations :: M.Map F.Name (DeclContext, FU.SrcSpan)
    -- | Map of unique names.
  , criticalsUniqMap      :: M.Map F.Name F.Name
    -- | Location of criticals.
  , criticalsFromWhere    :: M.Map F.Name FilePath
  }

instance Show Criticals where
  show crits =
    case vars of
      []   -> concat ["\n", fname, ": No additional annotations are necessary.\n"]
      _    -> concat ["\n", fname, ": ", show numVars
                     , " variable declarations suggested to be given a specification:\n"
                     , unlines [ "    " ++ declReport d | d <- M.toList dmapSlice ]]
      where
        fname        = F.pfGetFilename . criticalsPf $ crits
        dmap         = criticalsDeclarations           crits
        uniqnameMap  = criticalsUniqMap                crits
        fromWhereMap = criticalsFromWhere              crits
        vars         = criticalsVariables              crits
        unitVarName (UnitVar (v, _))                 = v
        unitVarName (UnitParamVarUse (_, (v, _), _)) = v
        unitVarName _                                = "<bad>"
        varNames  = map unitVarName vars
        dmapSlice = M.filterWithKey (\ k _ -> k `elem` varNames) dmap
        numVars   = M.size dmapSlice
        declReport (v, (_, ss)) = vfilename ++ " (" ++ showSpanStart ss ++ ")    " ++ fromMaybe v (M.lookup v uniqnameMap)
          where vfilename = fromMaybe fname $ M.lookup v fromWhereMap
                showSpanStart (FU.SrcSpan l _) = show l

-- | Identifies the variables that need to be annotated in order for
-- inference or checking to work.
criticalVariables :: Constraints -> [UnitInfo]
criticalVariables [] = []
criticalVariables cons = filter (not . isUnitRHS) $ map (colA A.!) criticalIndices
  where
    (unsolvedM, _, colA)          = constraintsToMatrix cons
    solvedM                       = rref unsolvedM
    uncriticalIndices             = mapMaybe (findIndex (/= 0)) $ H.toLists solvedM
    criticalIndices               = A.indices colA \\ uncriticalIndices
    isUnitRHS (UnitName _)        = True; isUnitRHS _ = False

-- | Return a list of critical variables as UnitInfo list (most likely
-- to be of the UnitVar constructor).
runCriticalVariables :: UnitSolver [UnitInfo]
runCriticalVariables = do
  cons <- usConstraints `fmap` get
  return $ criticalVariables cons

-- | Infer one possible set of critical variables for a program.
inferCriticalVariables :: UnitsAnalysis (F.ProgramFile Annotation) Criticals
inferCriticalVariables uOpts = do
  pf <- analysisInput
  let
    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = combinedModuleMap (uoModFiles uOpts)
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis . fmap UA.mkUnitAnnotation $ pf
    mmap' = extractModuleMap pfRenamed `M.union` mmap -- post-parsing
    -- unique name -> src name across modules

    -- Map of all declarations
    dmap = extractDeclMap pfRenamed `M.union` combinedDeclMap (uoModFiles uOpts)
    uniqnameMap = M.fromList [
                (FA.varName e, FA.srcName e) |
                e@(F.ExpValue _ _ F.ValVariable{}) <- universeBi pfRenamed :: [F.Expression UA]
                -- going to ignore intrinsics here
              ] `M.union` (M.unions . map (M.fromList . map (\ (a, (b, _)) -> (b, a)) . M.toList) $ M.elems mmap')
    (eVars, _, logs) = runInference uOpts pf runCriticalVariables
  writeDebug logs
  pure Criticals { criticalsPf           = pf
                 , criticalsVariables    = eVars
                 , criticalsDeclarations = dmap
                 , criticalsUniqMap      = uniqnameMap
                 , criticalsFromWhere    = fromWhereMap
                 }
  where
    fromWhereMap = genUniqNameToFilenameMap $ uoModFiles uOpts
