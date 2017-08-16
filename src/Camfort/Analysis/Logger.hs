{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wall            #-}

module Camfort.Analysis.Logger
  (
  -- * Conversion to text description
    Describe(..)
  -- * Messages
  , Origin
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
  , noOriginObj
  , LoggerT
  -- * Running a logger
  , LogOutput
  , logOutputStd
  , runLoggerT
  ) where

import           Data.Monoid                    ((<>))

import           Control.Lens

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Control.Monad.State            as Lazy
import           Control.Monad.State.Strict
import qualified Control.Monad.Writer           as Lazy
import           Control.Monad.Writer.Strict

import           Data.Text                      (Text)
import qualified Data.Text.IO                   as Text
import qualified Data.Text.Lazy                 as Lazy
import           Data.Text.Lazy.Builder         (Builder)
import qualified Data.Text.Lazy.Builder         as Builder

import qualified Language.Fortran.AST           as F
import qualified Language.Fortran.Util.Position as F

--------------------------------------------------------------------------------
--  'Describe' class
--------------------------------------------------------------------------------

class Describe a where
  describe :: a -> Text
  describeBuilder :: a -> Builder

  default describeBuilder :: Show a => a -> Builder
  describe = Lazy.toStrict . Builder.toLazyText . describeBuilder
  describeBuilder = Builder.fromString . show

instance Describe F.SrcSpan
instance Describe Text where
  describeBuilder = Builder.fromText

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
  deriving (Show, Eq, Functor)

makeLenses ''LogMessage

instance Describe a => Describe (LogMessage a) where
  describeBuilder msg =
    maybe "" (\origin -> " " <> describeBuilder origin) (msg ^. lmOrigin) <>
    ": " <> describeBuilder (msg ^. lmMsg)


data SomeMessage e w
  = MsgError (LogMessage e)
  | MsgWarn (LogMessage w)
  | MsgInfo (LogMessage Text)
  | MsgDebug (LogMessage Text)

makePrisms ''SomeMessage

--------------------------------------------------------------------------------
--  'MonadLogger' class
--------------------------------------------------------------------------------

-- TODO: Consider adding methods to change source file and current log level

class Monad m => MonadLogger e w m | m -> e w where
  createLogMessage :: (F.Spanned o) => Maybe o -> a -> m (LogMessage a)
  recordLogMessage :: SomeMessage e w -> m ()

  logGeneral :: (F.Spanned o) => (LogMessage a -> SomeMessage e w) -> Maybe o -> a -> m ()
  logGeneral mkMsg o =
    createLogMessage o >=>
    return . mkMsg >=>
    recordLogMessage

  logError ::
    (F.Spanned o) =>
    Maybe o -> e -> m ()
  logError = logGeneral MsgError

  logError' :: e -> m ()
  logError' = logError noOriginObj


  logWarn :: (F.Spanned o) => Maybe o -> w -> m ()
  logWarn = logGeneral MsgWarn

  logWarn' :: w -> m ()
  logWarn' = logWarn noOriginObj


  logInfo :: (F.Spanned o) => Maybe o -> Text -> m ()
  logInfo = logGeneral MsgInfo

  logInfo' :: Text -> m ()
  logInfo' = logInfo noOriginObj


  logDebug :: (F.Spanned o) => Maybe o -> Text -> m ()
  logDebug = logGeneral MsgDebug

  logDebug' :: Text -> m ()
  logDebug' = logDebug noOriginObj

  default createLogMessage :: (MonadTrans t, MonadLogger e w m', F.Spanned o, m ~ t m') => Maybe o -> a -> m (LogMessage a)
  default recordLogMessage :: (MonadTrans t, MonadLogger e w m', m ~ t m') => SomeMessage e w -> m ()

  createLogMessage o = lift . createLogMessage o
  recordLogMessage = lift . recordLogMessage

instance MonadLogger e w m => MonadLogger e w (ReaderT r m)
instance MonadLogger e w m => MonadLogger e w (ExceptT e' m)
instance MonadLogger e w m => MonadLogger e w (StateT s m)
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (WriterT w' m)
instance MonadLogger e w m => MonadLogger e w (Lazy.StateT s m)
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (Lazy.WriterT w' m)

noOriginObj :: Maybe (F.Expression ())
noOriginObj = Nothing

--------------------------------------------------------------------------------
--  'LoggerT' monad
--------------------------------------------------------------------------------

data LoggerState m e w =
  LoggerState
  { _lsMessages   :: ![SomeMessage e w]
  , _lsLogError   :: !(LogMessage e -> m ())
  , _lsLogWarn    :: !(LogMessage w -> m ())
  , _lsLogInfo    :: !(LogMessage Text -> m ())
  , _lsLogDebug   :: !(LogMessage Text -> m ())
  , _lsSourceFile :: !FilePath
  }

makeLenses ''LoggerState

newtype LoggerT e w m a = LoggerT (StateT (LoggerState m e w) m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader r
    , MonadWriter w'
    , MonadError e'
    )

instance MonadTrans (LoggerT e w) where
  lift = LoggerT . lift

instance (MonadState s m) => MonadState s (LoggerT e w m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (Monad m) => MonadLogger e w (LoggerT e w m) where
  recordLogMessage msg = do
    LoggerT $ lsMessages %= (msg :)
    putSomeMessage msg

  createLogMessage originObject msg = do
    sourceFile <- LoggerT $ use lsSourceFile

    let origin = Origin sourceFile . F.getSpan <$> originObject
    return $ LogMessage origin msg


type LogOutput m = Text -> m ()

logOutputStd :: MonadIO m => LogOutput m
logOutputStd = liftIO . Text.putStrLn


runLoggerT
  :: (Monad m, Describe e, Describe w)
  => FilePath -> LogOutput m -> LogLevel -> LoggerT e w m a -> m (a, [SomeMessage e w])
runLoggerT sourceFile output logLevel (LoggerT action) = do
  let st = setLogLevel' output logLevel $
        LoggerState
        { _lsMessages = []
        , _lsLogError = const (return ())
        , _lsLogWarn = const (return ())
        , _lsLogInfo = const (return ())
        , _lsLogDebug = const (return ())
        , _lsSourceFile = sourceFile
        }

  (x, st') <- runStateT action st

  return (x, st' ^. lsMessages)

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

setLogLevel'
  :: (Describe e, Describe w, Monad m)
  => LogOutput m -> LogLevel -> LoggerState m e w -> LoggerState m e w
setLogLevel' (output :: LogOutput m) lvl =
  let putMessage :: Describe a => Builder -> a -> m ()
      putMessage prefix msg =
        output . Lazy.toStrict . Builder.toLazyText $ prefix <> describeBuilder msg

      noOp = const $ return ()
      le = putMessage "ERROR"
      lw = if lvl >= LogWarn then putMessage "WARN" else noOp
      li = if lvl >= LogInfo then putMessage "INFO" else noOp
      ld = if lvl >= LogDebug then putMessage "DEBUG" else noOp
  in (lsLogError .~ le) .
     (lsLogWarn .~ lw) .
     (lsLogInfo .~ li) .
     (lsLogDebug .~ ld)


putSomeMessage :: (Monad m) => SomeMessage e w -> LoggerT e w m ()
putSomeMessage msg = do
  st <- LoggerT get
  lift $ case msg of
    MsgError m -> view lsLogError st m
    MsgWarn m  -> view lsLogWarn st m
    MsgDebug m -> view lsLogDebug st m
    MsgInfo m  -> view lsLogInfo st m
