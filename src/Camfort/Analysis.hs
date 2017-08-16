{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wall            #-}

{- |
Module      :  Camfort.Analysis
Description :  Analysis on fortran files.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental

This module defines functionality for aiding in analysing fortran files.
-}

module Camfort.Analysis
  (
  -- * Analysis monad
    AnalysisT
  , PureAnalysis
  , FileAnalysis
  , PureFileAnalysis
  -- * Early exit
  , failAnalysis
  , failAnalysis'
  -- * Analysis results
  , AnalysisResult(..)
  , _ARFailure
  , _ARSuccess
  , AnalysisReport(..)
  , arMessages
  , arResult
  -- * Running analyses
  , runFileAnalysis
  , runFileAnalysisPure
  -- * Logging
  , MonadLogger
    ( logError
    , logError'
    , logWarn
    , logWarn'
    , logInfo
    , logInfo'
    , logDebug
    , logDebug'
    )
  , atSpanned
  , atSpannedInFile
  , LogOutput
  , logOutputStd
  , Describe(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Control.Monad.Writer.Class

import           Control.Lens

import qualified Language.Fortran.AST           as F
import qualified Language.Fortran.Util.ModFile  as MF
import qualified Language.Fortran.Util.Position as F

import           Camfort.Analysis.Logger

--------------------------------------------------------------------------------
--  Analysis Monad
--------------------------------------------------------------------------------

newtype AnalysisT e w m a =
  AnalysisT
  { getAnalysisT ::
      ExceptT (LogMessage e) (LoggerT e w m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader r
    , MonadState s
    , MonadWriter w'
    , MonadLogger e w
    )

type PureAnalysis e w = AnalysisT e w Identity

type FileAnalysis ann e w m a = MF.ModFiles -> F.ProgramFile ann -> AnalysisT e w m a
type PureFileAnalysis ann e w a = MF.ModFiles -> F.ProgramFile ann -> PureAnalysis e w a

instance MonadTrans (AnalysisT e w) where
  lift = AnalysisT . lift . lift

instance MonadError e' m => MonadError e' (AnalysisT e w m) where
  throwError = lift . throwError
  catchError action handle = AnalysisT . ExceptT $
    let run = runExceptT . getAnalysisT
    in catchError (run action) (run . handle)

--------------------------------------------------------------------------------
--  Early exit
--------------------------------------------------------------------------------

-- | Report a critical error in the analysis at a particular source location
-- and exit early.
failAnalysis :: (Monad m) => Origin -> e -> AnalysisT e w m a
failAnalysis origin e = do
  let msg = LogMessage origin e
  recordLogMessage (MsgError msg)
  AnalysisT (throwError msg)

-- | Report a critical failure in the analysis at no particular source location
-- and exit early.
failAnalysis' :: (Monad m, F.Spanned o) => o -> e -> AnalysisT e w m a
failAnalysis' originElem e = do
  origin <- atSpanned originElem
  failAnalysis origin e

--------------------------------------------------------------------------------
--  Analysis Results
--------------------------------------------------------------------------------

data AnalysisResult e r
  = ARFailure Origin e
  | ARSuccess r

makePrisms ''AnalysisResult

data AnalysisReport e w r =
  AnalysisReport
  { _arMessages :: [SomeMessage e w]
  , _arResult   :: AnalysisResult e r
  }

makeLenses ''AnalysisReport

--------------------------------------------------------------------------------
--  Running Analyses
--------------------------------------------------------------------------------

-- | Run a file analysis with an arbitrary underlying monad (e.g. 'IO').
runFileAnalysis
  :: (Monad m, Describe e, Describe w)
  => MF.ModFiles
  -> F.ProgramFile ann
  -> LogOutput m
  -- ^ e.g. 'logOutputStd' to log to standard output
  -> LogLevel
  -> FileAnalysis ann e w m a
  -> m (AnalysisReport e w a)
runFileAnalysis modfiles pf output logLevel analysis = do
  let fileName = F.pfGetFilename pf

  (res1, messages) <-
    runLoggerT fileName output logLevel $
    runExceptT $
    getAnalysisT $
    analysis modfiles pf

  let result = case res1 of
        Left (LogMessage origin e) -> ARFailure origin e
        Right x                    -> ARSuccess x

  return $ AnalysisReport
    { _arMessages = messages
    , _arResult = result
    }

-- | Run a pure file analysis. Don't output the logs as we go along. Messages
-- are collected for later inspection.
runFileAnalysisPure
  :: (Describe e, Describe w)
  => MF.ModFiles
  -> F.ProgramFile ann
  -> (PureFileAnalysis ann e w a)
  -> AnalysisReport e w a
runFileAnalysisPure modfiles pf =
  runIdentity . runFileAnalysis modfiles pf (const (return ())) LogInfo
