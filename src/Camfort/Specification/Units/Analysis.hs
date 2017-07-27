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
  , runUnitsAnalysis
  ) where

import Language.Fortran.Util.ModFile (ModFiles)

import Camfort.Analysis.Fortran
  (Analysis, AnalysisResult, runAnalysis)
import Camfort.Specification.Units.Monad (UnitOpts)

-- | Analysis with access to 'UnitOpts' information.
type UnitsAnalysis a a' = Analysis UnitOpts () a a'

runUnitsAnalysis :: UnitsAnalysis a b -> UnitOpts -> ModFiles -> a -> AnalysisResult () b
runUnitsAnalysis analysis uo = runAnalysis analysis uo ()
