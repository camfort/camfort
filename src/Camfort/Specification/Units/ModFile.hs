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
  ( initializeModFiles
  ) where

import           Data.Binary (Binary, decodeOrFail)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Data (Data)
import qualified Data.Map.Strict            as M
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Language.Fortran.Util.ModFile

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
