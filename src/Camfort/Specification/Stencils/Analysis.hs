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
  , runStencilsAnalysis
  ) where

import qualified Language.Fortran.Analysis          as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.Types    as FAT
import qualified Language.Fortran.Util.ModFile      as MF

import qualified Camfort.Analysis as AF
import           Camfort.Analysis.Annotations (Report)
import           Camfort.Analysis.ModFile (MFCompiler)

type StencilsAnalysis a a' = AF.SimpleAnalysis a a'

-- | Run a 'StencilsAnalysis' analysis.
runStencilsAnalysis :: StencilsAnalysis a a' -> MF.ModFiles -> a -> AF.AnalysisResult Report () a'
runStencilsAnalysis analysis = AF.runAnalysis analysis () ()

-- | Compile a program to a 'ModFile' containing stencils information.
compileStencils :: MFCompiler ()
compileStencils () mfs pf =
  let
    -- Use the module map derived from all of the included Camfort Mod files.
    mmap = MF.combinedModuleMap mfs
    tenv = MF.combinedTypeEnv mfs
    pfRenamed = FAR.analyseRenamesWithModuleMap mmap . FA.initAnalysis $ pf
    pfTyped = fst . FAT.analyseTypesWithEnv tenv $ pfRenamed
  in MF.genModFile pfTyped
