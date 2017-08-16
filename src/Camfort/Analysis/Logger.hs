{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wall            #-}

module Camfort.Analysis.Logger
  (
  -- * Conversion to text description
    Describe(..)
  -- * Messages
  , Origin
  , LogLevel
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
  , LoggerT
  -- * Running a logger
  , LogOutput
  , logOutputStdout
  , runLoggerT
  ) where

import           Data.Monoid                    ((<>))

import           Control.Lens

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.Text                      (Text)
import qualified Data.Text.IO                   as Text
import qualified Data.Text.Lazy                 as Lazy
import           Data.Text.Lazy.Builder         (Builder)
import qualified Data.Text.Lazy.Builder         as Builder

import qualified Language.Fortran.AST           as F
import qualified Language.Fortran.Util.Position as F


class Describe a where
  describe :: a -> Text
  describeBuilder :: a -> Builder

  default describeBuilder :: Show a => a -> Builder
  describe = Lazy.toStrict . Builder.toLazyText . describeBuilder
  describeBuilder = Builder.fromString . show

instance Describe F.SrcSpan
instance Describe Text where
  describeBuilder = Builder.fromText


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

-- TODO: Consider adding methods to change source file and current log level

class MonadLogger e w m | m -> e w where
  logGeneral :: (F.Spanned o) => (LogMessage a -> SomeMessage e w) -> Maybe o -> a -> m ()

  logError ::
    (F.Spanned o) =>
    Maybe o -> e -> m ()
  logError = logGeneral MsgError

  logError' :: e -> m ()
  logError' = logError (Nothing :: Maybe (F.Expression ()))


  logWarn :: (F.Spanned o) => Maybe o -> w -> m ()
  logWarn = logGeneral MsgWarn

  logWarn' :: w -> m ()
  logWarn' = logWarn (Nothing :: Maybe (F.Expression ()))


  logInfo :: (F.Spanned o) => Maybe o -> Text -> m ()
  logInfo = logGeneral MsgInfo

  logInfo' :: Text -> m ()
  logInfo' = logInfo (Nothing :: Maybe (F.Expression ()))


  logDebug :: (F.Spanned o) => Maybe o -> Text -> m ()
  logDebug = logGeneral MsgDebug

  logDebug' :: Text -> m ()
  logDebug' = logDebug (Nothing :: Maybe (F.Expression ()))


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
    , MonadReader r
    , MonadWriter w'
    )

instance MonadTrans (LoggerT e w) where
  lift = LoggerT . lift

instance (MonadState s m) => MonadState s (LoggerT e w m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (Monad m) => MonadLogger e w (LoggerT e w m) where
  logGeneral mkMsg originObject msg = do
    sourceFile <- LoggerT $ use lsSourceFile

    let origin = Origin sourceFile . F.getSpan <$> originObject
        fullMsg = mkMsg (LogMessage origin msg)

    LoggerT $ lsMessages %= (fullMsg :)

    putSomeMessage fullMsg


type LogOutput m = Text -> m ()

logOutputStdout :: MonadIO m => LogOutput m
logOutputStdout = liftIO . Text.putStrLn


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
setLogLevel' output lvl =
  let putMessage prefix msg =
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
