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
  ) where

import Camfort.Analysis.Fortran (Analysis)

-- | Analysis with additional debugging information via a 'Report'.
type UnitsAnalysis a a' = Analysis a a'
