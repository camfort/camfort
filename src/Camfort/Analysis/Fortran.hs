{- |
Module      :  Camfort.Analysis.Fortran
Description :  Analysis on fortran files.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental

This module defines functionality for aiding in analysing fortran files.
-}

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Camfort.Analysis.Fortran
  ( Analysis
  , AnalysisResult
  , SimpleAnalysis
  , analysisDebug
  , analysisInput
  , analysisModFiles
  , analysisParams
  , analysisResult
  , branchAnalysis
  , finalState
  , runAnalysis
  , runSimpleAnalysis
  , writeDebug
  ) where

import Control.Monad.Reader (MonadReader, Reader, asks, runReader)
import Control.Monad.State  (MonadState, StateT, get, runStateT)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)

import qualified Language.Fortran.Util.ModFile as MF

import Camfort.Analysis.Annotations (Report)

-- | Data available to all analysiss.
data AnalysisEnv r a = AnalysisEnv
  {
    -- | fortran-src mod files for the current project.
    aEnvModFiles :: MF.ModFiles
    -- | The input data.
  , aEnvInput    :: a
    -- | Additional environment data.
  , aEnvEnv      :: r
  }

-- | Create a new 'AnalysisEnv'.
mkAnalysisEnv :: r -> MF.ModFiles -> a -> AnalysisEnv r a
mkAnalysisEnv r mfs x =
  AnalysisEnv { aEnvModFiles = mfs, aEnvInput = x, aEnvEnv = r }

-- | An @Analysis r s a a'@ performs an analysis on an
-- @a@ to produce a result of type @a'@. The analysis
-- may additionally access read-only information of type
-- @r@ and read-write information of type @s@.
newtype Analysis r s a a' = Analysis
  { getAnalysis :: WriterT Report (StateT s (Reader (AnalysisEnv r a))) a' }
  deriving ( Functor, Applicative, Monad
           , MonadReader (AnalysisEnv r a)
           , MonadState s
           , MonadWriter Report)

-- | An 'Analysis' without any additional state or parameters.
type SimpleAnalysis a a' = Analysis () () a a'

data AnalysisResult s a =
  AR { -- | The result of an analysis.
       analysisResult :: a
       -- | The final state after the analysis has been performed.
     , finalState     :: s
       -- | Debugging information produced in the analysis.
     , analysisDebug :: Report
     }

-- | Retrieve the state of the analysis.
analysisState :: Analysis r s a s
analysisState = get

-- | Retrieve the additional parameters to the analysis.
analysisParams :: Analysis r s a r
analysisParams = asks aEnvEnv

-- | Run the given analysis.
runAnalysis :: Analysis r s a a' -> r -> s -> MF.ModFiles -> a -> AnalysisResult s a'
runAnalysis a r s mfs x =
  let ((result, report), state) = runReader (runStateT (runWriterT $ getAnalysis a) s) env
  in AR { analysisResult = result
        , analysisDebug  = report
        , finalState     = state
        }
  where env = mkAnalysisEnv r mfs x

-- | Run a 'SimpleAnalysis'.
runSimpleAnalysis :: SimpleAnalysis a a' -> MF.ModFiles -> a -> AnalysisResult () a'
runSimpleAnalysis analysis = runAnalysis analysis () ()

-- | Run a separate analysis with a new input, but maintaining other information.
branchAnalysis :: Analysis r s b b' -> b -> Analysis r s a (AnalysisResult s b')
branchAnalysis branch input = do
  modFiles <- analysisModFiles
  env      <- analysisParams
  state    <- analysisState
  pure $ runAnalysis branch env state modFiles input

-- | Retrieve the input data to the analysis.
analysisInput :: Analysis r s a a
analysisInput = asks aEnvInput

-- | Retrieve the ModFiles available in the analysis.
analysisModFiles :: Analysis r s a MF.ModFiles
analysisModFiles = asks aEnvModFiles

-- | Write some debugging information.
writeDebug :: Report -> Analysis r s a ()
writeDebug = tell
