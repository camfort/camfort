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
  , generalizeLogger
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

withSpannedOrigin :: (MonadLogger e w m, F.Spanned a) => (Origin -> b -> m c) -> a -> b -> m c
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
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (RWST r w' s m)
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (Lazy.RWST r w' s m)

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

ungeneralizeState :: (Monad n) => LoggerState m e w -> LoggerState n e w
ungeneralizeState ls =
  LoggerState
  { _lsMessages = ls ^. lsMessages
  , _lsLogError = const (return ())
  , _lsLogWarn = const (return ())
  , _lsLogInfo = const (return ())
  , _lsLogDebug = const (return ())
  , _lsDefaultSourceFile = ls ^. lsDefaultSourceFile
  }

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
    logSomeMessage msg


-- | Generalize a pure logger to an arbitrary monad. Notice the input logger
-- cannot print any logs directly because it is a pure computation. It still
-- collects logs.
generalizeLogger :: (Monad m) => LoggerT e w Identity a -> LoggerT e w m a
generalizeLogger (LoggerT x) = do
  ls <- LoggerT get
  let (res, ls') = runState x (ungeneralizeState ls)
  LoggerT $ put (ls & lsMessages .~ ls' ^. lsMessages & lsDefaultSourceFile .~ ls' ^. lsDefaultSourceFile)
  return res


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


logSomeMessage :: (Monad m) => SomeMessage e w -> LoggerT e w m ()
logSomeMessage msg = do
  st <- LoggerT get
  lift $ case msg of
    MsgError m -> view lsLogError st m
    MsgWarn m  -> view lsLogWarn st m
    MsgDebug m -> view lsLogDebug st m
    MsgInfo m  -> view lsLogInfo st m
