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

import Camfort.Analysis.Fortran (SimpleAnalysis)
import Camfort.Specification.Units.Monad (UnitOpts)

-- | Analysis with access to 'UnitOpts' information.
type UnitsAnalysis a a' = UnitOpts -> SimpleAnalysis a a'
