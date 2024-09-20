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

import           Camfort.Analysis
import           Camfort.Analysis.Annotations
import           Camfort.Analysis.ModFile (withCombinedModuleMap)
import           Camfort.Specification.Units.Analysis (UnitAnalysis, runInference)
import qualified Camfort.Specification.Units.Annotation as UA
import           Camfort.Specification.Units.Environment
import           Camfort.Specification.Units.InferenceBackendSBV (criticalVariables)
import           Camfort.Specification.Units.Monad
import           Control.DeepSeq
import           Control.Monad.Reader (asks, lift, liftIO)
import           Control.Monad.State (get)
import           Data.Generics.Uniplate.Operations
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import           Language.Fortran.Util.ModFile
import qualified Language.Fortran.Util.Position as FU

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
    -- | Filename location of the declared variables
  , criticalsFromWhere    :: M.Map F.Name FilePath
  }

instance NFData Criticals where
  rnf _ = ()

instance ExitCodeOfReport Criticals where
  exitCodeOf _ = 0

instance Show Criticals where
  -- Key routine for showing the critical variables information
  show crits =
    case criticalVars of
      []   -> concat ["\n", fname, ": No additional annotations are necessary.\n"]
      _    -> concat ["\n", fname, ": ", show numVars
                     , " variable declarations suggested to be given a specification:\n"
                     , unlines [ "    " ++ declReport d | d <- M.toList dmapSubset ]]
      where
        -- Deconstructed components
        fname        = F.pfGetFilename . criticalsPf $ crits
        dmap         = criticalsDeclarations           crits
        uniqnameMap  = criticalsUniqMap                crits
        fromWhereMap = criticalsFromWhere              crits
        criticalVars  = criticalsVariables             crits

        -- Extract the variable source name for those constraints we want to see
        unitVarName (UnitVar (uniqName, _))                 = Just uniqName
        unitVarName (UnitParamVarUse (_, (uniqName, _), _)) = Just uniqName
        unitVarName (UnitParamPosAbs (_, (uniqName, _), _)) = Just uniqName
        unitVarName _                                       = Nothing

        -- Just the variable names asociated to constraint variables we are interested in
        criticalVarNames  = mapMaybe unitVarName criticalVars

        -- Filter out the declaration map for all those variable names we are interested in
        dmapSubset = M.filterWithKey (\k _ -> k `elem` criticalVarNames) dmap

        -- Size of the critical set
        numVars   = M.size dmapSubset -- should be same as M.size criticalVarNames

        -- Generate report
        declReport (v, (_, ss)) =
          vfilename ++ ":" ++ showSpanStart ss ++ "    " ++ fromMaybe v (M.lookup v uniqnameMap)
          where vfilename = fromMaybe fname $ M.lookup v fromWhereMap
                showSpanStart (FU.SrcSpan l _) = show l

instance Describe Criticals

-- | Infer one possible set of critical variables for a program.
-- \ (depends on first doing inference)
inferCriticalVariables :: FilePath -> UnitAnalysis Criticals
inferCriticalVariables localPath = do
  pf <- asks unitProgramFile
  mfs <- lift analysisModFiles
  (eVars, _) <- runInference runCriticalVariables
  let
    -- Use the module map derived from all of the included Camfort Mod files.
    (pfRenamed, mmap) = withCombinedModuleMap mfs . FA.initAnalysis . fmap UA.mkUnitAnnotation $ pf
    -- unique name -> src name across modules

    -- Map of all declarations
    dmap = M.map project $ extractDeclMap pfRenamed `M.union` combinedDeclMap mfs
    project (dc, _, srcSpan) = (dc, srcSpan)

    -- renaming map
    uniqnameMap = M.fromList [
                (FA.varName e, FA.srcName e) |
                e@(F.ExpValue _ _ F.ValVariable{}) <- universeBi pfRenamed :: [F.Expression UA]
                -- going to ignore intrinsics here
              ] `M.union` (M.unions . map (M.fromList . map (\ (a, (b, _)) -> (b, a)) . M.toList) $ M.elems mmap)
    -- get unique map of names from the filenameMap too
    uniqnameMap' = M.fromList $ mapMaybe extractSrcName (M.toList filenameMap)
    extractSrcName (n, (_, Nothing))      = Nothing
    extractSrcName (n, (_, Just srcName)) = Just (n, srcName)

    -- filenameMap
    fromWhereMap = M.map fst filenameMap
    filenameMap = genUniqNameToFilenameMap localPath mfs
--  (show eVars) `trace`
  pure $!! Criticals { criticalsPf           = pf
                     , criticalsVariables    = eVars
                     , criticalsDeclarations = dmap
                     , criticalsUniqMap      = uniqnameMap `M.union` uniqnameMap'
                     , criticalsFromWhere    = fromWhereMap
                     }

-- (not used)
-- | Return a list of critical variables as UnitInfo list (most likely
-- to be of the UnitVar constructor).
runCriticalVariables :: UnitSolver [UnitInfo]
runCriticalVariables = do
  cons <- usConstraints `fmap` get
  return $ criticalVariables cons
