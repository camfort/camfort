{- |
Module      :  Camfort.Specification.Stencils.Analysis
Description :  Helpers for generic stencils analysis.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

module Camfort.Specification.Stencils.Analysis
  ( StencilsAnalysis
  ) where

import Camfort.Analysis.Fortran (Analysis)

type StencilsAnalysis a a' = Analysis a a'
