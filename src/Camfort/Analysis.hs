{- |
Module      :  Camfort.Analysis
Description :  Analysis on fortran files.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental

This module defines functionality for aiding in analysing fortran files.
-}

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Camfort.Analysis
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

import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State  (MonadState, StateT, get, runStateT)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import Control.Monad.IO.Class (MonadIO(..))

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
-- @r@, read-write information of type @s@, and may log
-- information of type @d@.
newtype Analysis r d s a a' = Analysis
  { getAnalysis :: WriterT d (StateT s (ReaderT (AnalysisEnv r a) IO)) a' }
  deriving ( Functor, Applicative, Monad
           , MonadReader (AnalysisEnv r a)
           , MonadState s
           , MonadWriter d
           , MonadIO
           )

-- | An 'Analysis' without any additional state or parameters.
type SimpleAnalysis = Analysis () Report ()

data AnalysisResult d s a =
  AR { -- | The result of an analysis.
       analysisResult :: a
       -- | The final state after the analysis has been performed.
     , finalState     :: s
       -- | Debugging information produced in the analysis.
     , analysisDebug :: d
     }

-- | Retrieve the state of the analysis.
analysisState :: (Monoid d) => Analysis r d s a s
analysisState = get

-- | Retrieve the additional parameters to the analysis.
analysisParams :: (Monoid d) => Analysis r d s a r
analysisParams = asks aEnvEnv

-- | Run the given analysis.
runAnalysis :: (Monoid d) => Analysis r d s a a' -> r -> s -> MF.ModFiles -> a -> IO (AnalysisResult d s a')
runAnalysis a r s mfs x = do
  let env = mkAnalysisEnv r mfs x
  ((result, report), state) <- runReaderT (runStateT (runWriterT $ getAnalysis a) s) env
  return $ AR { analysisResult = result
              , analysisDebug  = report
              , finalState     = state
              }

-- | Run a 'SimpleAnalysis'.
runSimpleAnalysis :: SimpleAnalysis a a' -> MF.ModFiles -> a -> IO (AnalysisResult Report () a')
runSimpleAnalysis analysis = runAnalysis analysis () ()

-- | Run a separate analysis with a new input, but maintaining other information.
branchAnalysis :: (Monoid d) => Analysis r d s b b' -> b -> Analysis r d s a (AnalysisResult d s b')
branchAnalysis branch input = do
  modFiles <- analysisModFiles
  env      <- analysisParams
  state    <- analysisState
  liftIO $ runAnalysis branch env state modFiles input

-- | Retrieve the input data to the analysis.
analysisInput :: (Monoid d) => Analysis r d s a a
analysisInput = asks aEnvInput

-- | Retrieve the ModFiles available in the analysis.
analysisModFiles :: (Monoid d) => Analysis r d s a MF.ModFiles
analysisModFiles = asks aEnvModFiles

-- | Write some debugging information.
writeDebug :: (Monoid d) => d -> Analysis r d s a ()
writeDebug = tell
