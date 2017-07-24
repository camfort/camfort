{- |
Module      :  Camfort.Specification.Units.Analysis
Description :  Helpers for units refactoring and analysis.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

module Camfort.Specification.Units.Analysis
  ( UnitsAnalysis
  , UnitsRefactoring
  ) where

import Camfort.Analysis
import Camfort.Analysis.Annotations (Report)

-- | Analysis with additional debugging information via a 'Report'.
type UnitsAnalysis r a       = Analysis    (Report,       r) a
type UnitsRefactoring r a a' = Refactoring (Report, Maybe r) a a'
