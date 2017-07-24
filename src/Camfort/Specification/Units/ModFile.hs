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
    CompiledUnits
  , cuNameParamMap
  , cuTemplateMap
  , initializeModFiles
  , genUnitsModFile
  , runCompileUnits
  ) where

import           Control.Monad.State (get, gets)
import           Data.Binary (Binary, decodeOrFail, encode)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Data (Data)
import           Data.Generics.Uniplate.Operations (universeBi)
import           Data.List (partition)
import qualified Data.Map.Strict            as M
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import qualified Language.Fortran.AST as F
import           Language.Fortran.Util.ModFile

import Camfort.Specification.Units.Annotation (UA)
import Camfort.Specification.Units.Environment (UnitInfo(..))
import Camfort.Specification.Units.InferenceBackend (flattenUnits, genUnitAssignments)
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
  Just bs -> case decodeOrFail (LB.fromStrict bs) of
    Left _ -> emptyCompiledUnits
    Right (_, _, cu) -> cu

-- | Initialize units-relevant ModFile information.
initializeModFiles :: ModFiles -> UnitSolver ()
initializeModFiles mfs = do
  let compiledUnits = combinedCompiledUnits   $ mfs
  modifyTemplateMap  . const . cuTemplateMap  $ compiledUnits
  modifyNameParamMap . const . cuNameParamMap $ compiledUnits

-- | Produce information for a "units-mod" file.
runCompileUnits :: UnitSolver CompiledUnits
runCompileUnits = do
  cons <- usConstraints `fmap` get

  -- Sketching some ideas about solving the unit equation for each
  -- parameter of each function.
  let unitAssigns = map (fmap flattenUnits) $ genUnitAssignments cons
  let mulCons x = map (\ (UnitPow u k) -> UnitPow u (x * k))
  let negateCons = mulCons (-1)
  let epsilon = 0.001 -- arbitrary
  let approxEq a b = abs (b - a) < epsilon
  let uninvert ([UnitPow u k], rhs) | not (k `approxEq` 1) = ([UnitPow u 1], mulCons (1 / k) rhs)
      uninvert (lhs, rhs)                                  = (lhs, rhs)
  let shiftTerms name pos (lhs, rhs) = (lhsOk ++ negateCons rhsShift, rhsOk ++ negateCons lhsShift)
        where
          (lhsOk, lhsShift) = partition isLHS lhs
          (rhsOk, rhsShift) = partition (not . isLHS) rhs
          isLHS (UnitParamPosAbs (n, i)) | n == name && i == pos = True
          isLHS (UnitPow u _) = isLHS u
          isLHS _ = False

  let nameParams = M.fromList [ (NPKParam name pos, rhs) | assign <- unitAssigns
                                                         , UnitParamPosAbs (name, pos) <- universeBi assign
                                                         , let (_, rhs) = uninvert $ shiftTerms name pos assign ]


  let variables = M.fromList [ (NPKVariable var, units) | ([UnitPow (UnitVar var) k], units) <- unitAssigns
                                                        , k `approxEq` 1 ]

  tmap <- gets usTemplateMap
  pure CompiledUnits { cuTemplateMap = tmap, cuNameParamMap = nameParams `M.union` variables }

genUnitsModFile :: F.ProgramFile UA -> CompiledUnits -> ModFile
genUnitsModFile pf cu = alterModFileData f unitsCompiledDataLabel $ genModFile pf
  where
    f _ = Just . LB.toStrict $ encode cu
