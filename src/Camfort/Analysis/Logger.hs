{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wall            #-}

module Camfort.Analysis.Logger
  (
  -- * Conversion to text description
    Describe(..)
  , tellDescribe
  , describeShow
  , builderToStrict
  , Builder
  , Text
  , (<>)
  -- * Messages
  , Origin(..)
  , oFile
  , oSpan
  , LogLevel(..)
  , LogMessage(..)
  , lmOrigin
  , lmMsg
  , SomeMessage(..)
  , _MsgError
  , _MsgWarn
  , _MsgInfo
  , _MsgDebug
  -- * Logging monad
  , MonadLogger(..)
  , atSpanned
  , atSpannedInFile
  , LoggerT
  -- * Running a logger
  , LogOutput
  , logOutputStd
  , runLoggerT
  ) where

import           Data.Monoid                    ((<>))
import           Data.Void                      (Void)

import           Control.Lens

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Control.Monad.RWS              as Lazy
import           Control.Monad.RWS.Strict       (RWST)
import qualified Control.Monad.State            as Lazy
import           Control.Monad.State.Strict
import qualified Control.Monad.Writer           as Lazy
import           Control.Monad.Writer.Strict
import           Control.Monad.Morph

import           Data.Text                      (Text)
import qualified Data.Text.IO                   as Text
import qualified Data.Text.Lazy                 as Lazy
import           Data.Text.Lazy.Builder         (Builder)
import qualified Data.Text.Lazy.Builder         as Builder

import qualified Language.Fortran.Util.Position as F

--------------------------------------------------------------------------------
--  'Describe' class
--------------------------------------------------------------------------------

class Describe a where
  describe :: a -> Text
  describeBuilder :: a -> Builder

  default describeBuilder :: Show a => a -> Builder
  describe = builderToStrict . describeBuilder
  describeBuilder = Builder.fromString . show

instance Describe F.SrcSpan
instance Describe Text where
  describeBuilder = Builder.fromText
instance Describe [Char] where
  describeBuilder = Builder.fromString
instance Describe () where
  describeBuilder = const mempty
instance Describe Int
instance Describe Integer
instance Describe Float
instance Describe Double
instance Describe Void

builderToStrict :: Builder -> Text
builderToStrict = Lazy.toStrict . Builder.toLazyText

tellDescribe :: (MonadWriter Builder m, Describe a) => a -> m ()
tellDescribe = tell . describeBuilder

describeShow :: (Show a) => a -> Text
describeShow = describe . show

--------------------------------------------------------------------------------
--  Messages
--------------------------------------------------------------------------------

data Origin =
  Origin
  { _oFile :: FilePath
  , _oSpan :: F.SrcSpan
  }
  deriving (Show, Eq)

makeLenses ''Origin

instance Describe Origin where
  describeBuilder origin =
    "at [" <> Builder.fromString (origin ^. oFile) <>
    ", " <> describeBuilder (origin ^. oSpan) <> "]"


data LogLevel
  = LogError
  | LogWarn
  | LogInfo
  | LogDebug
  deriving (Show, Eq, Ord)

instance Describe LogLevel where
  describeBuilder LogError = "ERROR"
  describeBuilder LogWarn  = "WARN"
  describeBuilder LogInfo  = "INFO"
  describeBuilder LogDebug = "DEBUG"


data LogMessage a =
  LogMessage
  { _lmOrigin :: Maybe Origin
  , _lmMsg    :: a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

makeLenses ''LogMessage

instance Describe a => Describe (LogMessage a) where
  describeBuilder msg =
    maybe "" describeBuilder (msg ^. lmOrigin) <>
    ": " <> describeBuilder (msg ^. lmMsg)


data SomeMessage e w
  = MsgError (LogMessage e)
  | MsgWarn (LogMessage w)
  | MsgInfo (LogMessage Text)
  | MsgDebug (LogMessage Text)
  deriving (Show, Eq)

makePrisms ''SomeMessage

instance (Describe e, Describe w) => Describe (SomeMessage e w) where
  describeBuilder msg = case msg of
    MsgError m -> "ERROR: " <> describeBuilder m
    MsgWarn  m -> "WARN: "  <> describeBuilder m
    MsgInfo  m -> "INFO: "  <> describeBuilder m
    MsgDebug m -> "DEBUG: " <> describeBuilder m

--------------------------------------------------------------------------------
--  'MonadLogger' class
--------------------------------------------------------------------------------

atSpanned :: (MonadLogger e w m, F.Spanned a) => a -> m Origin
atSpanned astElem = do
  sf <- getDefaultSourceFile
  let sp = F.getSpan astElem
  return $ Origin sf sp

withSpannedOrigin
  :: (MonadLogger e w m, F.Spanned a)
  => (Origin -> b -> m c) -> a -> b -> m c
withSpannedOrigin f x m = do
  origin <- atSpanned x
  f origin m

atSpannedInFile :: (F.Spanned a) => FilePath -> a -> Origin
atSpannedInFile sf = Origin sf . F.getSpan

-- TODO: Consider methods to change current log level

class Monad m => MonadLogger e w m | m -> e w where
  setDefaultSourceFile :: FilePath -> m ()
  getDefaultSourceFile :: m FilePath

  recordLogMessage :: SomeMessage e w -> m ()

  logGeneral :: (LogMessage a -> SomeMessage e w) -> Origin -> a -> m ()
  logGeneral mkMsg origin msg =
    recordLogMessage (mkMsg (LogMessage (Just origin) msg))

  logError :: Origin -> e -> m ()
  logError = logGeneral MsgError

  logError' :: (F.Spanned a) => a -> e -> m ()
  logError' = withSpannedOrigin logError


  logWarn :: Origin -> w -> m ()
  logWarn = logGeneral MsgWarn

  logWarn' :: (F.Spanned a) => a -> w -> m ()
  logWarn' = withSpannedOrigin logWarn


  logInfo :: Origin -> Text -> m ()
  logInfo = logGeneral MsgInfo

  logInfo' :: (F.Spanned a) => a -> Text -> m ()
  logInfo' = withSpannedOrigin logInfo

  logInfoNoOrigin :: Text -> m ()
  logInfoNoOrigin msg = recordLogMessage (MsgInfo (LogMessage Nothing msg))


  logDebug :: Origin -> Text -> m ()
  logDebug = logGeneral MsgDebug

  logDebug' :: (F.Spanned a) => a -> Text -> m ()
  logDebug' = withSpannedOrigin logDebug

  default recordLogMessage
    :: (MonadTrans t, MonadLogger e w m', m ~ t m') => SomeMessage e w -> m ()
  default setDefaultSourceFile
    :: (MonadTrans t, MonadLogger e w m', m ~ t m') => FilePath -> m ()
  default getDefaultSourceFile
    :: (MonadTrans t, MonadLogger e w m', m ~ t m') => m FilePath

  recordLogMessage = lift . recordLogMessage
  setDefaultSourceFile = lift . setDefaultSourceFile
  getDefaultSourceFile = lift getDefaultSourceFile

instance MonadLogger e w m => MonadLogger e w (ReaderT r m)
instance MonadLogger e w m => MonadLogger e w (ExceptT e' m)
instance MonadLogger e w m => MonadLogger e w (StateT s m)
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (WriterT w' m)
instance MonadLogger e w m => MonadLogger e w (Lazy.StateT s m)
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (Lazy.WriterT w' m)
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (RWST r w' s m)
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (Lazy.RWST r w' s m)

--------------------------------------------------------------------------------
--  'LoggerT' monad
--------------------------------------------------------------------------------

data LoggerState e w =
  LoggerState
  { _lsMessages          :: ![SomeMessage e w]
  , _lsLogLevel :: LogLevel
  , _lsDefaultSourceFile :: !FilePath
  }

data LoggerEnv m =
  LoggerEnv
  { _leLogFunc :: !(LogLevel -> LogLevel -> Text -> m ())
  }

makeLenses ''LoggerState
makeLenses ''LoggerEnv

hoistEnv :: (m () -> n ()) -> LoggerEnv m -> LoggerEnv n
hoistEnv f = leLogFunc %~ \logFunc l1 l2 msg -> f $ logFunc l1 l2 msg

newtype LoggerT e w m a =
  LoggerT (
    ReaderT (LoggerEnv m) (
    StateT (LoggerState e w)
    m) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadWriter w'
    , MonadError e'
    )

instance MonadTrans (LoggerT e w) where
  lift = LoggerT . lift . lift

instance (MonadState s m) => MonadState s (LoggerT e w m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (MonadReader r m) => MonadReader r (LoggerT e w m) where
  ask = lift ask
  local f (LoggerT (ReaderT k)) = LoggerT $ ReaderT $ local f . k

instance (Monad m, Describe e, Describe w) =>
         MonadLogger e w (LoggerT e w m) where
  setDefaultSourceFile = LoggerT . (lsDefaultSourceFile .=)
  getDefaultSourceFile = LoggerT (use lsDefaultSourceFile)

  recordLogMessage msg = do
    LoggerT $ lsMessages %= (msg :)
    logSomeMessage msg


-- | This doesn't behave quite as you may think. When a 'LoggerT' is hoisted,
-- the resulting 'LoggerT' cannot output as it goes. It still collects logs to
-- be inspected when it finishes.
instance MFunctor (LoggerT e w) where
  hoist f (LoggerT (ReaderT k)) = LoggerT $ ReaderT $ \env ->
    let env' = hoistEnv (const (return ())) env
    in hoist f (k env')


type LogOutput m = Text -> m ()

logOutputStd :: MonadIO m => LogOutput m
logOutputStd = liftIO . Text.putStrLn


runLoggerT
  :: (Monad m, Describe e, Describe w)
  => FilePath -> LogOutput m -> LogLevel
  -> LoggerT e w m a
  -> m (a, [SomeMessage e w])
runLoggerT sourceFile output logLevel (LoggerT action) = do
  let st = LoggerState
        { _lsMessages = []
        , _lsLogLevel = logLevel
        , _lsDefaultSourceFile = sourceFile
        }

      env = LoggerEnv { _leLogFunc = logFuncFrom output }

  (x, st') <- runStateT (runReaderT action env) st

  return (x, st' ^. lsMessages)

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

logFuncFrom
  :: (Monad m)
  => LogOutput m
  -> (LogLevel -> LogLevel -> Text -> m ())
logFuncFrom output = lf
  where
    lf maxLevel level msg
      | level <= maxLevel = output msg
      | otherwise = return ()


someLogLevel :: SomeMessage e w -> LogLevel
someLogLevel (MsgError _) = LogError
someLogLevel (MsgWarn _) = LogWarn
someLogLevel (MsgInfo _) = LogInfo
someLogLevel (MsgDebug _) = LogDebug


logSomeMessage
  :: (Monad m, Describe e, Describe w)
  => SomeMessage e w -> LoggerT e w m ()
logSomeMessage msg = do
  let msgText = describe msg
      msgLevel = someLogLevel msg

  logFunc <- LoggerT $ view leLogFunc
  logLevel <- LoggerT $ use lsLogLevel

  lift $ logFunc logLevel msgLevel msgText
