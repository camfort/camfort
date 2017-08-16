{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -Wall            #-}

module Camfort.Analysis.Logging
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
  -- * Logging
  , Logger
  , HasLogger(..)
  , makeLogger
  , logError
  , logError'
  , logWarn
  , logWarn'
  , logDebug
  , logDebug'
  , logInfo
  , logInfo'
  ) where

import           Data.IORef
import           Data.Monoid                    ((<>))

import           Control.Lens

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Text                      (Text)
import qualified Data.Text.IO                   as Text
import qualified Data.Text.Lazy                 as Lazy
import           Data.Text.Lazy.Builder         (Builder)
import qualified Data.Text.Lazy.Builder         as Builder

import qualified Language.Fortran.AST as F
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
  = SomeMsgError (LogMessage e)
  | SomeMsgWarn (LogMessage w)
  | SomeMsgInfo (LogMessage Text)
  | SomeMsgDebug (LogMessage Text)


data LoggerState e w =
  LoggerState
  { _lsMessages :: ![SomeMessage e w]
  , _lsLogError :: !(LogMessage e -> IO ())
  , _lsLogWarn  :: !(LogMessage w -> IO ())
  , _lsLogInfo  :: !(LogMessage Text -> IO ())
  , _lsLogDebug :: !(LogMessage Text -> IO ())
  }

makeLenses ''LoggerState


data Logger e w =
  Logger
  { _lAnalysisName :: !Text
  , _lSourceFile   :: !FilePath
  , _lState        :: !(IORef (LoggerState e w))
  }

makeLenses ''Logger


makeLogger :: (Describe e, Describe w) => Text -> FilePath -> LogLevel -> IO (Logger e w)
makeLogger analysisName sourceFile logLevel = do
  let st = setLogLevel' logLevel $
        LoggerState
        { _lsMessages = []
        , _lsLogError = const (return ())
        , _lsLogWarn = const (return ())
        , _lsLogInfo = const (return ())
        , _lsLogDebug = const (return ())
        }

  stRef <- newIORef st

  return $ Logger
    { _lAnalysisName = analysisName
    , _lSourceFile = sourceFile
    , _lState = stRef
    }


class HasLogger e w r | r -> e w where
  {-# MINIMAL getLogger | logger #-}

  getLogger :: r -> Logger e w
  getLogger = view logger

  logger :: Getter r (Logger e w)
  logger = to getLogger

instance HasLogger e w (Logger e w) where
  getLogger = id


logError ::
  (F.Spanned o, MonadIO m, MonadReader r m, HasLogger e w r) =>
  Maybe o -> e -> m ()
logError = logGeneral SomeMsgError

logError' :: (MonadIO m, MonadReader r m, HasLogger e w r) => e -> m ()
logError' = logError (Nothing :: Maybe (F.Expression ()))


logWarn ::
  (F.Spanned o, MonadIO m, MonadReader r m, HasLogger e w r) =>
  Maybe o -> w -> m ()
logWarn = logGeneral SomeMsgWarn

logWarn' :: (MonadIO m, MonadReader r m, HasLogger e w r) => w -> m ()
logWarn' = logWarn (Nothing :: Maybe (F.Expression ()))


logInfo ::
  (F.Spanned o, MonadIO m, MonadReader r m, HasLogger e w r) =>
  Maybe o -> Text -> m ()
logInfo = logGeneral SomeMsgInfo

logInfo' :: (MonadIO m, MonadReader r m, HasLogger e w r) => Text -> m ()
logInfo' = logInfo (Nothing :: Maybe (F.Expression ()))


logDebug ::
  (F.Spanned o, MonadIO m, MonadReader r m, HasLogger e w r) =>
  Maybe o -> Text -> m ()
logDebug = logGeneral SomeMsgDebug

logDebug' :: (MonadIO m, MonadReader r m, HasLogger e w r) => Text -> m ()
logDebug' = logDebug (Nothing :: Maybe (F.Expression ()))


--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

logGeneral
  :: (HasLogger e w r, MonadReader r m, MonadIO m, F.Spanned o)
  => (LogMessage a -> SomeMessage e w) -> Maybe o -> a -> m ()
logGeneral mkMsg originObject msg = do
  sourceFile <- view (logger . lSourceFile)

  let origin = Origin sourceFile . F.getSpan <$> originObject
      fullMsg = mkMsg (LogMessage origin msg)

  stRef <- view (logger . lState)
  withIORef stRef $ do
    lsMessages %= (fullMsg :)
    st <- get
    liftIO $ putSomeMessage fullMsg st

withIORef :: (MonadIO m) => IORef s -> StateT s m a -> m a
withIORef ref action = do
  s <- liftIO $ readIORef ref
  (x, s') <- runStateT action s
  liftIO $ writeIORef ref s'
  return x

putMessage :: (Describe m) => Text -> LogMessage m -> IO ()
putMessage prefix msg = do
  Text.putStr prefix
  Text.putStrLn (describe msg)


setLogLevel' :: (Describe e, Describe w) => LogLevel -> LoggerState e w -> LoggerState e w
setLogLevel' lvl =
  let noOp = const $ return ()
      le = putMessage "ERROR"
      lw = if lvl >= LogWarn then putMessage "WARN" else noOp
      li = if lvl >= LogInfo then putMessage "INFO" else noOp
      ld = if lvl >= LogDebug then putMessage "DEBUG" else noOp
  in (lsLogError .~ le) .
     (lsLogWarn .~ lw) .
     (lsLogInfo .~ li) .
     (lsLogDebug .~ ld)


putSomeMessage :: SomeMessage e w -> LoggerState e w -> IO ()
putSomeMessage msg st =
  case msg of
    SomeMsgError m -> view lsLogError st m
    SomeMsgWarn m  -> view lsLogWarn st m
    SomeMsgDebug m -> view lsLogDebug st m
    SomeMsgInfo m  -> view lsLogInfo st m
