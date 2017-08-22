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
  , compileStencils
  ) where

import qualified Language.Fortran.Util.ModFile as MF

import           Camfort.Analysis
import           Camfort.Analysis.ModFile      (MFCompiler, simpleCompiler)

-- TODO:
-- type StencilsAnalysis = PureAnalysis StencilCheckError StencilCheckWarning

type StencilsAnalysis = PureAnalysis () ()

-- | Compile a program to a 'ModFile' containing stencils information.
compileStencils :: Monad m => MFCompiler () m
compileStencils = simpleCompiler
