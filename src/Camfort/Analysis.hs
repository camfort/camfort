{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wall            #-}

{- |
Module      :  Camfort.Analysis
Description :  Analysis on fortran files.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental

This module defines the 'AnalysisT' monad transformer, which encapsulates common
functionality for analyses:

- Logging via the 'MonadLogger' class
- Early exit via 'failAnalysis' or 'failAnalysis\''
- Error recovery via 'catchAnalysisT' or 'loggingAnalysisError'
- Providing access to the analysis environment via 'analysisModFiles'

-}

module Camfort.Analysis
  (
  -- * Analysis monad
    AnalysisT
  , PureAnalysis
  -- * Combinators
  , mapAnalysisT
  , generalizePureAnalysis
  , MonadAnalysis(..)
  , failAnalysis'
  , catchAnalysisT
  , loggingAnalysisError
  , analysisLiftLogger
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
  -- * Logging
  -- | See "Camfort.Analysis.Logger" for more detailed documentation.

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
  -- ** Message origins
  , Origin(..)
  , atSpanned
  , atSpannedInFile
  -- ** Log outputs
  , LogOutput
  , logOutputStd
  , logOutputNone
  -- ** Log levels
  , LogLevel(..)
  -- ** 'Describe' class
  , Describe(..)
  , describeShow
  , (<>)
  ) where

import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import qualified Control.Monad.RWS              as Lazy
import           Control.Monad.RWS.Strict
import qualified Control.Monad.State            as Lazy
import           Control.Monad.State.Strict
import qualified Control.Monad.Writer           as Lazy
import           Control.Monad.Writer.Strict

import           Control.Lens

import qualified Data.Text.Lazy                 as Lazy
import qualified Data.Text.Lazy.Builder         as Builder
import qualified Data.Text.Lazy.IO              as Lazy

import qualified Language.Fortran.Util.ModFile  as F
import qualified Language.Fortran.Util.Position as F

import           Camfort.Analysis.Logger

--------------------------------------------------------------------------------
--  Analysis Monad
--------------------------------------------------------------------------------

-- | The analysis monad transformer. Will usually be based on 'Identity' (see
-- 'PureAnalysis') or 'IO'.
--
-- Has error messages of type @e@ and warnings of type @w@.
newtype AnalysisT e w m a =
  AnalysisT
  { getAnalysisT ::
      ExceptT (LogMessage e) (ReaderT F.ModFiles (LoggerT e w m)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState s
    , MonadWriter w'
    , MonadLogger e w
    )

-- | A pure analysis computation which cannot do any 'IO'.
type PureAnalysis e w = AnalysisT e w Identity

instance MonadTrans (AnalysisT e w) where
  lift = AnalysisT . lift . lift . lift

-- | As per the 'MFunctor' instance for 'LoggerT', a hoisted analysis cannot
-- output logs on the fly.
instance MFunctor (AnalysisT e w) where
  hoist f (AnalysisT x) = AnalysisT (hoist (hoist (hoist f)) x)

instance MonadError e' m => MonadError e' (AnalysisT e w m) where
  throwError = lift . throwError
  catchError action handle = AnalysisT . ExceptT $
    let run = runExceptT . getAnalysisT
    in catchError (run action) (run . handle)

instance MonadReader r m => MonadReader r (AnalysisT e w m) where
  ask = lift ask

  local f (AnalysisT (ExceptT (ReaderT k))) =
    AnalysisT . ExceptT . ReaderT $ local f . k

--------------------------------------------------------------------------------
--  Liftable functions
--------------------------------------------------------------------------------

class (MonadLogger e w m) => MonadAnalysis e w m where
  -- | Report a critical error in the analysis at a particular source location
  -- and exit early.
  failAnalysis :: Origin -> e -> m a

  -- | Get the 'F.ModFiles' from the analysis environment.
  analysisModFiles :: m F.ModFiles

  default failAnalysis
    :: (MonadTrans t, MonadAnalysis e w m', m ~ t m') => Origin -> e -> m a

  default analysisModFiles
    :: (MonadTrans t, MonadAnalysis e w m', m ~ t m') => m F.ModFiles

  failAnalysis o = lift . failAnalysis o
  analysisModFiles = lift analysisModFiles

instance (Describe e, Describe w, Monad m) => MonadAnalysis e w (AnalysisT e w m) where
  analysisModFiles = AnalysisT ask

  failAnalysis origin e = do
    let msg = LogMessage (Just origin) e
    AnalysisT (throwError msg)

instance MonadAnalysis e w m => MonadAnalysis e w (ReaderT r m)
instance MonadAnalysis e w m => MonadAnalysis e w (ExceptT e' m)
instance MonadAnalysis e w m => MonadAnalysis e w (StateT s m)
instance (MonadAnalysis e w m, Monoid w') => MonadAnalysis e w (WriterT w' m)
instance MonadAnalysis e w m => MonadAnalysis e w (Lazy.StateT s m)
instance (MonadAnalysis e w m, Monoid w') => MonadAnalysis e w (Lazy.WriterT w' m)
instance (MonadAnalysis e w m, Monoid w') => MonadAnalysis e w (RWST r w' s m)
instance (MonadAnalysis e w m, Monoid w') => MonadAnalysis e w (Lazy.RWST r w' s m)

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------

-- | Change the error and warning types in an analysis. To change the
-- underlying monad use 'hoist'.
mapAnalysisT :: (Monad m) => (e -> e') -> (w -> w') -> AnalysisT e w m a -> AnalysisT e' w' m a
mapAnalysisT mapError mapWarn =
  AnalysisT .
  (hoist (hoist (mapLoggerT mapError mapWarn)) . withExceptT (over lmMsg mapError)) .
  getAnalysisT

-- | Given a pure analysis action, it can be generalized to run in any 'Monad'.
-- Since the original analysis was pure, it could not have logged anything as it
-- ran. The new analysis cannot log anything as it runs either, even it is based
-- on 'IO'.
generalizePureAnalysis :: (Monad m) => PureAnalysis e w a -> AnalysisT e w m a
generalizePureAnalysis = hoist generalize

-- | Report a critical failure in the analysis at no particular source location
-- and exit early.
failAnalysis'
  :: (MonadAnalysis e w m, F.Spanned o)
  => o -> e -> m a
failAnalysis' originElem e = do
  origin <- atSpanned originElem
  failAnalysis origin e

-- | Run the given analysis and recover with the given handler function if it
-- fails.
catchAnalysisT
  :: (Monad m)
  => (LogMessage e -> AnalysisT e w m a) -> AnalysisT e w m a -> AnalysisT e w m a
catchAnalysisT handle action =
  AnalysisT (catchError (getAnalysisT action) (getAnalysisT . handle))

-- | Run the given analysis. If it succeeds, return its result value. Otherwise,
-- log the error it creates and return 'Nothing'.
--
-- This allows errors in analysis sub-programs to be collected rather than
-- halting the entire analysis.
loggingAnalysisError
  :: (Monad m, Describe w, Describe e)
  => AnalysisT e w m a -> AnalysisT e w m (Maybe a)
loggingAnalysisError =
  catchAnalysisT ( fmap (const Nothing)
                 . recordLogMessage
                 . MsgError)
  . fmap Just

-- | Given a logging computation, lift it into an analysis monad.
analysisLiftLogger
  :: (Monad m, Describe w, Describe e)
  => LoggerT e w m a -> AnalysisT e w m a
analysisLiftLogger = AnalysisT . lift . lift

--------------------------------------------------------------------------------
--  Analysis Results
--------------------------------------------------------------------------------

data AnalysisResult e r
  = ARFailure Origin e
  | ARSuccess r
  deriving (Show, Eq, Functor)

makePrisms ''AnalysisResult

-- | When an analysis is run, it produces a report consisting of the logs it
-- collect as it ran. In addition, it either fails at a certain location or
-- succeeds with a result value.
data AnalysisReport e w r =
  AnalysisReport
  { _arSourceFile :: !FilePath
  , _arMessages   :: ![SomeMessage e w]
  , _arResult     :: !(AnalysisResult e r)
  }
  deriving (Show, Eq, Functor)

makeLenses ''AnalysisReport

-- | Produce a human-readable version of an 'AnalysisReport', at the given
-- verbosity level. Giving 'Nothing' for the log level hides all logs.
describeReport
  :: (Describe e, Describe w, Describe r)
  => Text -> Maybe LogLevel -> AnalysisReport e w r -> Lazy.Text
describeReport analysisName level report = Builder.toLazyText . execWriter $ do
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
  tell "Finished running "
  tellDescribe analysisName
  tell " on input: "
  tellDescribe (report ^. arSourceFile)
  tell "\n"

  -- Output logs if requested
  case level of
    Just lvl -> do
      tell $ "Logs:\n"
      forM_ (report ^. arMessages) (describeMessage lvl)
      tell "\n"
      tell "Result:\n"
    Nothing -> return ()

  let loggedWarnings = arMessages . traverse . _MsgWarn
      loggedErrors = arMessages . traverse . _MsgError

      hadErrors = notNullOf loggedErrors report
      hadWarnings = notNullOf loggedWarnings report

  case report ^. arResult of
    ARFailure origin e -> do
      tell $ "CRITICAL ERROR:\n"
      tell $ describeBuilder origin
      tell ": "
      tell $ describeBuilder e
    ARSuccess r -> do
      tell $ case (hadErrors, hadWarnings) of
        (True, _) -> "Finished, but with errors:"
        (False, True) -> "Finished, but with warnings:"
        (False, False) -> "OK:"
      tell "\n"
      tell $ describeBuilder r


putDescribeReport
  :: (Describe e, Describe w, Describe r, MonadIO m)
  => Text -> Maybe LogLevel -> AnalysisReport e w r -> m ()
putDescribeReport analysisName level = liftIO . Lazy.putStrLn . describeReport analysisName level


--------------------------------------------------------------------------------
--  Running Analyses
--------------------------------------------------------------------------------

-- | Run an analysis computation and collect the report.
runAnalysisT
  :: (Monad m, Describe e, Describe w)
  => FilePath
  -- ^ The name of the file the analysis is being run on. This is only used for
  -- logging.
  -> LogOutput m
  -- ^ The logging output function, e.g. 'logOutputStd' for standard output or
  -- 'logOutputNone' for no output.
  -> LogLevel
  -- ^ The logging verbosity level.
  -> F.ModFiles
  -- ^ The list of analysis modfiles.
  -> AnalysisT e w m a
  -- ^ The analysis transformer to run.
  -> m (AnalysisReport e w a)
runAnalysisT fileName output logLevel mfs analysis = do

  (res1, messages) <-
    runLoggerT fileName output logLevel .
    flip runReaderT mfs .
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
