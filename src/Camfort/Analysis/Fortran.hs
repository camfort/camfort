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
  , analysisDebug
  , analysisInput
  , analysisResult
  , branchAnalysis
  , runAnalysis
  , writeDebug
  ) where

import Control.Monad.Reader (MonadReader, Reader, ask, runReader)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)

import qualified Language.Fortran.Util.ModFile as MF

import Camfort.Analysis.Annotations (Report)

-- | Data available to all analysiss.
data AnalysisEnv a = AnalysisEnv
  {
    -- | fortran-src mod files for the current project.
    aEnvModFiles :: MF.ModFiles
    -- | The input data.
  , aEnvInput    :: a
  }

-- | Create a new 'AnalysisEnv'.
mkAnalysisEnv :: MF.ModFiles -> a -> AnalysisEnv a
mkAnalysisEnv mfs x =
  AnalysisEnv { aEnvModFiles = mfs, aEnvInput = x }

-- | An @Analysis a a'@ analysis an @a@ to produce
-- a result of type @a'@.
newtype Analysis a a' = Analysis
  { getAnalysis :: WriterT Report (Reader (AnalysisEnv a)) a' }
  deriving ( Functor, Applicative, Monad
           , MonadReader (AnalysisEnv a)
           , MonadWriter Report)

newtype AnalysisResult a = AR { getAnalysisResult :: (a, Report) }

-- | Retrieve the result of an analysis.
analysisResult :: AnalysisResult a -> a
analysisResult = fst . getAnalysisResult

-- | Retrieve debugging information from an analysis.
analysisDebug :: AnalysisResult a -> Report
analysisDebug = snd . getAnalysisResult

-- | Run the given analysis.
runAnalysis :: Analysis a a' -> MF.ModFiles -> a -> AnalysisResult a'
runAnalysis r mfs x = AR $ runReader (runWriterT (getAnalysis r)) env
  where env = mkAnalysisEnv mfs x

-- | Run a separate analysis with a new input, but maintaining other information.
branchAnalysis :: Analysis b b' -> b -> Analysis a (AnalysisResult b')
branchAnalysis branch input = do
  modFiles <- analysisModFiles
  pure $ runAnalysis branch modFiles input

-- | Retrieve the input data to the analysis.
analysisInput :: Analysis a a
analysisInput = fmap aEnvInput ask

-- | Retrieve the ModFiles available in the analysis.
analysisModFiles :: Analysis a MF.ModFiles
analysisModFiles = fmap aEnvModFiles ask

-- | Write some debugging information.
writeDebug :: Report -> Analysis a ()
writeDebug = tell
