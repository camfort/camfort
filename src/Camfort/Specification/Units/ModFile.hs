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

module Camfort.Specification.Units.ModFile
  (
    genUnitsModFile
  , initializeModFiles
  , runCompileUnits
  ) where

import           Control.Monad.State (get, gets, lift)
import           Data.Binary (Binary, decodeOrFail, encode)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Data (Data)
import           Data.Generics.Uniplate.Operations (universeBi)
import           Data.List (partition)
import qualified Data.Map                   as M
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import qualified Language.Fortran.AST as F
import           Language.Fortran.Util.ModFile

import Camfort.Analysis (analysisModFiles)
import Camfort.Specification.Units.Annotation (UA)
import Camfort.Specification.Units.Environment (Constraint(..), foldUnits, UnitInfo(..), colSort)
import Camfort.Specification.Units.InferenceBackend (flattenConstraints, flattenUnits, genUnitAssignments, genUnitAssignments')
import Camfort.Specification.Units.Monad

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

  let unitAssigns = flattenConstraints cons
  let epsilon = 0.001 -- arbitrary
  let approxEq a b = abs (b - a) < epsilon

  let variables = M.fromList [ (NPKVariable var, units) | ([UnitPow (UnitVar var) k], units) <- unitAssigns
                                                        , k `approxEq` 1 ]

  tmap <- M.map optimiseTemplate `fmap` gets usTemplateMap
  let npm = variables

  -- D.traceM $ "npm = " ++ unlines (map show (M.toList npm))
  -- D.traceM $ "tmap = \n" ++ unlines [ f ++ "\n" ++ unlines (map show cons) | (f, cons) <- M.toList tmap ]
  pure CompiledUnits { cuTemplateMap = tmap, cuNameParamMap = variables }

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
