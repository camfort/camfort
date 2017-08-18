{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
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
  , describeReport
  , putDescribeReport
  -- * Running analyses
  , runAnalysisT
  , generalizePureAnalysis
  -- ** Logging
  , MonadLogger
    ( logError
    , logError'
    , logWarn
    , logWarn'
    , logInfo
    , logInfo'
    , logInfoNoOrigin
    , logDebug
    , logDebug'
    )
  -- * Messages origins
  , Origin
  , atSpanned
  , atSpannedInFile
  -- * Log outputs
  , LogOutput
  , logOutputStd
  -- * 'Describe' class
  , Describe(..)
  , describeShow
  , (<>)
  ) where

import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Control.Monad.Writer.Strict

import           Control.Lens

import qualified Data.Text.Lazy                 as Lazy
import qualified Data.Text.Lazy.Builder         as Builder
import qualified Data.Text.Lazy.IO              as Lazy

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
  let msg = LogMessage (Just origin) e
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
  deriving (Show, Eq, Functor)

makePrisms ''AnalysisResult

data AnalysisReport e w r =
  AnalysisReport
  { _arSourceFile :: !FilePath
  , _arMessages   :: ![SomeMessage e w]
  , _arResult     :: !(AnalysisResult e r)
  }
  deriving (Show, Eq, Functor)

makeLenses ''AnalysisReport

instance (Describe e, Describe r) => Describe (AnalysisResult e r) where
  describeBuilder (ARFailure origin e) =
    "CRITICAL ERROR " <> describeBuilder origin <> ": " <> describeBuilder e

  describeBuilder (ARSuccess r) =
    "OK: " <> describeBuilder r


describeReport :: (Describe e, Describe w, Describe r) => Maybe LogLevel -> AnalysisReport e w r -> Lazy.Text
describeReport level report = Builder.toLazyText . execWriter $ do
  let describeMessage lvl msg = do
        let tell' x = do
              tell " -"
              tellDescribe x
              tell "\n"
        case msg of
          m@(MsgError _) -> tell' m
          m@(MsgWarn  _) | lvl >= LogWarn -> tell' m
          m@(MsgInfo  _) | lvl >= LogInfo -> tell' m
          m@(MsgDebug _) | lvl >= LogDebug -> tell' m
          _              -> return ()

  -- Output file name
  tellDescribe (report ^. arSourceFile)
  tell "\n"

  -- Output logs if requested
  case level of
    Just lvl -> do
      tell $ "Logs:\n"
      forM_ (report ^. arMessages) (describeMessage lvl)
    Nothing -> return ()

  -- Output results
  tell "\n"
  tell "Result:\n"
  tell " -"
  tellDescribe (report ^. arResult)


putDescribeReport
  :: (Describe e, Describe w, Describe r, MonadIO m)
  => Maybe LogLevel -> AnalysisReport e w r -> m ()
putDescribeReport level = liftIO . Lazy.putStrLn . describeReport level


--------------------------------------------------------------------------------
--  Running Analyses
--------------------------------------------------------------------------------

-- | Run an analysis computation and collect the report.
runAnalysisT
  :: (Monad m, Describe e, Describe w)
  => FilePath
  -> LogOutput m
  -> LogLevel
  -> AnalysisT e w m a
  -> m (AnalysisReport e w a)
runAnalysisT fileName output logLevel analysis = do

  (res1, messages) <-
    runLoggerT fileName output logLevel .
    runExceptT .
    getAnalysisT $
    analysis

  let result = case res1 of
        Right x -> ARSuccess x
        Left (LogMessage (Just origin) e) -> ARFailure origin e
        Left _ -> error "impossible: failure without origin"

  return $ AnalysisReport
    { _arSourceFile = fileName
    , _arMessages = messages
    , _arResult = result
    }

-- | Generalize a pure analysis to run in an arbitrary monad. The input analysis
-- cannot output any logs directly because it is pure. It does collect them into
-- the report at the end, however.
generalizePureAnalysis :: (Monad m) => PureAnalysis e w a -> AnalysisT e w m a
generalizePureAnalysis = AnalysisT . hoist generalizeLogger . getAnalysisT
