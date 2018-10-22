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
  , dumpModFileCompiledUnits
  ) where

import           Control.Monad.State (get, gets, lift)
import           Data.Binary (Binary, decodeOrFail, encode)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Data (Data)
import           Data.Generics.Uniplate.Operations (universeBi)
import           Data.List (partition)
import           Data.Maybe (catMaybes)
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
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
  pf <- usProgramFile  `fmap` get

  let unitAssigns = flattenConstraints cons
  let epsilon = 0.001 -- arbitrary
  let approxEq a b = abs (b - a) < epsilon

  let variables = M.fromList [ (NPKVariable var, units) | ([UnitPow (UnitVar var) k], units) <- unitAssigns
                                                        , k `approxEq` 1 ]

  let getName pu | F.Named n <- FA.puName pu = Just n | otherwise = Nothing
  let puNameSet = S.fromList $ catMaybes [ getName pu | pu <- universeBi pf :: [F.ProgramUnit UA] ]
  let filterPUs = M.filterWithKey (const . (`S.member` puNameSet))
  let varNameSet = S.fromList $
        [ FA.varName v | F.DeclVariable _ _ v _ _ <- universeBi pf :: [F.Declarator UA] ] ++
        [ FA.varName v | F.DeclArray _ _ v _ _ _  <- universeBi pf :: [F.Declarator UA] ]
  let filterVarNames = M.filterWithKey (\ npk _ -> case npk of
                                           NPKVariable (v, _) -> v `S.member` varNameSet
                                           _                  -> False
                                       )
  tmap <- (filterPUs . M.map optimiseTemplate) `fmap` gets usTemplateMap
  let npm = filterVarNames variables

  -- D.traceM $ "npm = " ++ unlines (map show (M.toList npm))
  -- D.traceM $ "tmap = \n" ++ unlines [ f ++ "\n" ++ unlines (map show cons) | (f, cons) <- M.toList tmap ]
  pure CompiledUnits { cuTemplateMap = tmap, cuNameParamMap = npm }

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
                   , unlines [ i 2 (show uis ++ " :: " ++ show npk) | (npk, uis) <- M.toList (cuNameParamMap cu) ] ]
 where
   i n s = replicate n ' ' ++ s
