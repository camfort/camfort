{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
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
  , atSpanned
  , atSpannedInFile
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
  { _lmOrigin :: Origin
  , _lmMsg    :: a
  }
  deriving (Show, Eq, Functor)

makeLenses ''LogMessage

instance Describe a => Describe (LogMessage a) where
  describeBuilder msg =
    describeBuilder (msg ^. lmOrigin) <>
    ": " <> describeBuilder (msg ^. lmMsg)


data SomeMessage e w
  = MsgError (LogMessage e)
  | MsgWarn (LogMessage w)
  | MsgInfo (LogMessage Text)
  | MsgDebug (LogMessage Text)

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

atSpannedInFile :: (MonadLogger e w m, F.Spanned a) => FilePath -> a -> m Origin
atSpannedInFile sf = pure . Origin sf . F.getSpan

-- TODO: Consider methods to change current log level

class Monad m => MonadLogger e w m | m -> e w where
  setDefaultSourceFile :: FilePath -> m ()
  getDefaultSourceFile :: m FilePath

  recordLogMessage :: SomeMessage e w -> m ()

  logGeneral :: (LogMessage a -> SomeMessage e w) -> m Origin -> a -> m ()
  logGeneral mkMsg makeOrigin msg = do
    origin <- makeOrigin
    let message = LogMessage origin msg
    recordLogMessage (mkMsg message)

  logError ::
    m Origin -> e -> m ()
  logError = logGeneral MsgError

  logError' :: (F.Spanned a) => a -> e -> m ()
  logError' = logError . atSpanned


  logWarn :: m Origin -> w -> m ()
  logWarn = logGeneral MsgWarn

  logWarn' :: (F.Spanned a) => a -> w -> m ()
  logWarn' = logWarn . atSpanned


  logInfo :: m Origin -> Text -> m ()
  logInfo = logGeneral MsgInfo

  logInfo' :: (F.Spanned a) => a -> Text -> m ()
  logInfo' = logInfo . atSpanned


  logDebug :: m Origin -> Text -> m ()
  logDebug = logGeneral MsgDebug

  logDebug' :: (F.Spanned a) => a -> Text -> m ()
  logDebug' = logDebug . atSpanned

  default recordLogMessage :: (MonadTrans t, MonadLogger e w m', m ~ t m') => SomeMessage e w -> m ()
  default setDefaultSourceFile :: (MonadTrans t, MonadLogger e w m', m ~ t m') => FilePath -> m ()
  default getDefaultSourceFile :: (MonadTrans t, MonadLogger e w m', m ~ t m') => m FilePath

  recordLogMessage = lift . recordLogMessage
  setDefaultSourceFile = lift . setDefaultSourceFile
  getDefaultSourceFile = lift getDefaultSourceFile

instance MonadLogger e w m => MonadLogger e w (ReaderT r m)
instance MonadLogger e w m => MonadLogger e w (ExceptT e' m)
instance MonadLogger e w m => MonadLogger e w (StateT s m)
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (WriterT w' m)
instance MonadLogger e w m => MonadLogger e w (Lazy.StateT s m)
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (Lazy.WriterT w' m)

--------------------------------------------------------------------------------
--  'LoggerT' monad
--------------------------------------------------------------------------------

data LoggerState m e w =
  LoggerState
  { _lsMessages          :: ![SomeMessage e w]
  , _lsLogError          :: !(LogMessage e -> m ())
  , _lsLogWarn           :: !(LogMessage w -> m ())
  , _lsLogInfo           :: !(LogMessage Text -> m ())
  , _lsLogDebug          :: !(LogMessage Text -> m ())
  , _lsDefaultSourceFile :: !FilePath
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
  setDefaultSourceFile = LoggerT . (lsDefaultSourceFile .=)
  getDefaultSourceFile = LoggerT (use lsDefaultSourceFile)

  recordLogMessage msg = do
    LoggerT $ lsMessages %= (msg :)
    putSomeMessage msg


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
        , _lsDefaultSourceFile = sourceFile
        }

  (x, st') <- runStateT action st

  return (x, st' ^. lsMessages)

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

setLogLevel'
  :: (Describe e, Describe w, Monad m)
  => LogOutput m -> LogLevel -> LoggerState m e w -> LoggerState m e w
setLogLevel' output lvl =
  let putMessage msg =
        output . Lazy.toStrict . Builder.toLazyText $ describeBuilder msg

      noOp = const $ return ()
      le = putMessage . MsgError
      lw = if lvl >= LogWarn then putMessage . MsgWarn else noOp
      li = if lvl >= LogInfo then putMessage . MsgInfo else noOp
      ld = if lvl >= LogDebug then putMessage . MsgDebug else noOp
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
