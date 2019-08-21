{- |
Module      :  Camfort.Specification.Units.ModFile
Description :  Helpers for working with units-relevant ModFiles.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TupleSections #-}

module Camfort.Specification.Units.ModFile
  (
    genUnitsModFile
  , initializeModFiles
  , runCompileUnits
  , dumpModFileCompiledUnits
  ) where

import           Camfort.Analysis (analysisModFiles)
import           Camfort.Specification.Units.Annotation (UA)
import           Camfort.Specification.Units.Environment (Constraint(..), foldUnits, UnitInfo(..), colSort, Constraints)
import           Camfort.Specification.Units.InferenceBackend (flattenConstraints, flattenUnits, genUnitAssignments')
import           Camfort.Specification.Units.Monad
import           Control.Monad.State (get, gets, lift)
import           Data.Binary (Binary, decodeOrFail, encode)
import           Data.Data (Data)
import           Data.Generics.Uniplate.Operations (universeBi)
import           Data.List (partition)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Set as S
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import           Language.Fortran.Util.ModFile
import           Prelude hiding (mod)

-- | The data-structure stored in 'fortran-src mod files'
data CompiledUnits = CompiledUnits
  { cuTemplateMap  :: TemplateMap
  , cuNameParamMap :: NameParamMap
  } deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary CompiledUnits

emptyCompiledUnits :: CompiledUnits
emptyCompiledUnits = CompiledUnits M.empty M.empty

combinedCompiledUnits :: ModFiles -> CompiledUnits
combinedCompiledUnits mfs = CompiledUnits { cuTemplateMap = M.unions tmaps
                                          , cuNameParamMap = M.unions nmaps }
  where
    cus = map mfCompiledUnits mfs
    tmaps = map cuTemplateMap cus
    nmaps = map cuNameParamMap cus

-- | Name of the labeled data within a ModFile containing unit-specific info.
unitsCompiledDataLabel :: String
unitsCompiledDataLabel = "units-compiled-data"

mfCompiledUnits :: ModFile -> CompiledUnits
mfCompiledUnits mf = case lookupModFileData unitsCompiledDataLabel mf of
  Nothing -> emptyCompiledUnits
  Just bs -> case decodeOrFail bs of
    Left _ -> emptyCompiledUnits
    Right (_, _, cu) -> cu

-- | Initialize units-relevant ModFile information.
initializeModFiles :: UnitSolver ()
initializeModFiles = do
  mfs <- lift . lift $ analysisModFiles
  let compiledUnits = combinedCompiledUnits mfs
  modifyTemplateMap  . const . cuTemplateMap  $ compiledUnits
  modifyNameParamMap . const . cuNameParamMap $ compiledUnits

-- | Produce information for a "units-mod" file.
runCompileUnits :: UnitSolver CompiledUnits
runCompileUnits = do
  cons <- usConstraints `fmap` get
  pf <- usProgramFile  `fmap` get

  let unitAssigns = flattenConstraints cons
  let epsilon = 0.001 -- arbitrary
  let approxEq a b = abs (b - a) < epsilon

  let variables = M.fromList [ (NPKVariable var, units) | ([UnitPow (UnitVar var) k], units) <- unitAssigns
                                                        , k `approxEq` 1 ]

  -- Create sets of relevant program-unit and variable names.
  let getName pu | F.Named n <- FA.puName pu = Just n | otherwise = Nothing
  let puNameSet = S.fromList $ catMaybes [ getName pu | pu <- universeBi pf :: [F.ProgramUnit UA] ] ++
                               [ FA.varName e | F.BlInterface _ _ (Just e) _ _ _ <- universeBi pf :: [F.Block UA] ]

  let puVarNameSet pu = S.fromList $
        [ (FA.varName v, FA.srcName v) | F.DeclVariable _ _ v _ _ <- universeBi pu :: [F.Declarator UA] ] ++
        [ (FA.varName v, FA.srcName v) | F.DeclArray _ _ v _ _ _  <- universeBi pu :: [F.Declarator UA] ]

  -- Map of modules -> associated declared variables
  let puVarNameMap :: M.Map F.ProgramUnitName (S.Set VV)
      puVarNameMap = M.fromListWith S.union [ (F.getName pu, puVarNameSet pu)
                                            | pu@F.PUModule {} <- universeBi pf :: [F.ProgramUnit UA] ]

  -- Filter functions that cut out any information not having to do
  -- with the current modules being compiled.
  let filterPUs = M.filterWithKey (const . (`S.member` puNameSet))
  -- FIXME: investigate whether we should include vars that are linked
  -- transitively to current pf vars should also be included.
  tmap <- (filterPUs . M.map optimiseTemplate) `fmap` gets usTemplateMap

  let findNPK vv = ( (NPKVariable vv), ) <$> M.lookup (NPKVariable vv) variables

  -- 'Name Param Map': module names -> (variables -> unit info)
  let npm = M.filter (not . M.null) $ flip M.map puVarNameMap (M.fromList . mapMaybe findNPK . S.toList)

  pure CompiledUnits { cuTemplateMap = tmap, cuNameParamMap = npm }

-- | Cut out unnecessary constraints in the template using the solver.
optimiseTemplate :: Constraints -> Constraints
optimiseTemplate cons = map (\ (l, r) -> ConEq (foldUnits l) r) optimised
  where
    unitAssigns  = genUnitAssignments' (compileColSort) cons
    unitPows     = map (fmap flattenUnits) unitAssigns
    optimised    = filter cull $ map (fmap foldUnits . shiftTerms) unitPows

    cull (lhs, _) = or [ True | (UnitPow (UnitParamPosAbs _) _) <- universeBi lhs ]

    isUnitRHS (UnitPow (UnitName _) _)        = True
    isUnitRHS (UnitPow (UnitParamEAPAbs _) _) = True
    isUnitRHS (UnitPow (UnitParamImpAbs _) _) = True
    isUnitRHS (UnitPow (UnitParamPosAbs _) _) = False
    isUnitRHS _                               = False

    negateCons = map (\ (UnitPow u k) -> UnitPow u (-k))

    shiftTerms :: ([UnitInfo], [UnitInfo]) -> ([UnitInfo], [UnitInfo])
    shiftTerms (lhs, rhs) = (lhsOk ++ negateCons rhsShift, rhsOk ++ negateCons lhsShift)
      where
        (lhsOk, lhsShift) = partition (not . isUnitRHS) lhs
        (rhsOk, rhsShift) = partition isUnitRHS rhs

    compileColSort = flip colSort

-- | Generate a new ModFile containing Units information.
genUnitsModFile :: F.ProgramFile UA -> CompiledUnits -> ModFile
genUnitsModFile pf cu = alterModFileData f unitsCompiledDataLabel $ genModFile pf
  where
    f _ = Just $ encode cu

-- | Return information about compiled units for debugging purposes
dumpModFileCompiledUnits :: ModFile -> Maybe String
dumpModFileCompiledUnits mf = do
  bs <- lookupModFileData unitsCompiledDataLabel mf
  cu <- case decodeOrFail bs of
    Left _ -> Nothing
    Right (_, _, cu) -> Just cu
  pure . unlines $ [ "Template Map (size=" ++ show (M.size (cuTemplateMap cu)) ++ "):"
                   , concat [ unlines (i 2 fname':map (i 4 . show) temp)
                            | (fname, temp) <- M.toList (cuTemplateMap cu)
                            , let fname' = "Template for " ++ show fname ]
                   , "NameParam Map  (size=" ++ show (M.size (cuNameParamMap cu)) ++ "):"
                   , concat [ unlines (i 2 mod':[ i 4 (show ui ++ " :: " ++ show v)
                                                | (NPKVariable (v,_), ui) <- M.toList npkmap ])
                            | (mod, npkmap) <- M.toList (cuNameParamMap cu)
                            , let mod' = "Module " ++ show mod ]
                   ]
 where
   i n s = replicate n ' ' ++ s
