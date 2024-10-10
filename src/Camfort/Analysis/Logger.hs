{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE CPP                        #-}

{-# OPTIONS_GHC -Wall            #-}

{-|

Provides logging for analyses. 'MonadLogger' is a type class for monads which
support logging. 'LoggerT' is a concrete monad transformer instantiating the
class.

As a logger runs, it may print out log messages on the fly (depending on the
provided 'LogOutput' function). It also collects logs to be inspected at the end
of the computation.

A log message must usually include an 'Origin', which describes where in a
Fortran source file the message originated. This is made more convenient via
functions such as 'logWarn\'', which produces an 'Origin' based on a piece of
Fortran syntax, along with a default source file stored in the environment.

Log messages each come with an associated 'LogLevel':

- 'LogError' is for hard errors which will often cause the computation to fail.
- 'LogWarn' is for messages about things that are likely to cause problems.
- 'LogInfo' is for general information about what the computation is doing.
- 'LogDebug' is for extra-verbose output that helps with debugging, but which
  will be uninteresting to most users.

-}
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
  , ParsedOrigin(..)
  , parseOrigin
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
  , mapLoggerT
  -- * Running a logger
  , LogOutput
  , logOutputStd
  , logOutputNone
  , runLoggerT
  , formatError
  , formatSuccess
  ) where

import qualified Data.Semigroup                 as SG
import           Data.Void                      (Void)

import           Control.DeepSeq
import           Control.Lens

import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.RWS
import qualified Control.Monad.State            as Lazy
import           Control.Monad.State.Strict
import           Control.Monad.Writer

import           Data.Text                      (Text)
import qualified Data.Text.IO                   as Text
import qualified Data.Text.Lazy                 as Lazy
import           Data.Text.Lazy.Builder         (Builder)
import qualified Data.Text.Lazy.Builder         as Builder

import           GHC.Generics

import           Text.Read                      (readMaybe)
import qualified Language.Fortran.Util.Position as F

#if !MIN_VERSION_base(4,13,0)
-- Control.Monad.Fail import is redundant since GHC 8.8.1
import           Control.Monad.Fail
#endif

--------------------------------------------------------------------------------
--  'Describe' class
--------------------------------------------------------------------------------

-- TODO: More 'Describe' instances for built-in types.

-- | A type class for efficiently converting values to human-readable output.
-- Can be automatically instantiated for 'Show' types, but this will not be very
-- human-readable for a lot of types.
class Describe a where
  -- | Convert the value to a human-readable output as a strict 'Text' value.
  describe :: a -> Text

  -- | Convert the value to human-readable output in a text 'Builder' which can
  -- be efficiently concatenated with other 'Builder's.
  describeBuilder :: a -> Builder

  default describeBuilder :: Show a => a -> Builder
  describe = builderToStrict . describeBuilder
  describeBuilder = Builder.fromString . show

instance Describe F.SrcSpan
instance Describe F.Position
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

-- | A convenience combinator to directly convert a lazy text 'Builder' to a
-- strict 'Text' value.
builderToStrict :: Builder -> Text
builderToStrict = Lazy.toStrict . Builder.toLazyText

-- | Write a 'Describe'-able value directly into a writer monad.
tellDescribe :: (MonadWriter Builder m, Describe a) => a -> m ()
tellDescribe = tell . describeBuilder

-- | Convert a 'Show'-able value directly to strict 'Text'. Useful when you have
-- a 'Show' instance but not a 'Describe' instance.
describeShow :: (Show a) => a -> Text
describeShow = describe . show

--------------------------------------------------------------------------------
--  Messages
--------------------------------------------------------------------------------

-- | A message origin, containing a file and a source span.
data Origin =
  Origin
  { _oFile :: FilePath
  , _oSpan :: F.SrcSpan
  }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''Origin

instance NFData Origin

instance Describe Origin where
  describeBuilder origin =
    -- Present the file and span in a standard format for
    -- editor integration (link to source)
    "at [" <> Builder.fromString (origin ^. oFile) <>
    ":" <> describeBuilder startSpan <>
    " - " <> describeBuilder endSpan <> "]"
    where
      startSpan = F.ssFrom (origin ^. oSpan)
      endSpan   = F.ssFrom (origin ^. oSpan)

data ParsedOrigin = ParsedOrigin FilePath (Int, Int) (Int, Int)
  deriving (Show, Eq, Ord)

-- | Extract information about filename and source span from a string.
parseOrigin :: String -> Maybe ParsedOrigin
parseOrigin str
  | not (null filename)
  , Just (pos1, rest) <- parsePos comma
  , Just (pos2, _)    <- parsePos rest = Just (ParsedOrigin filename pos1 pos2)
  | otherwise                          = Nothing
  where
    lbrack            = dropWhile (/= '[') str
    (filename, comma) = break (==',') (drop 1 lbrack)

parsePos :: String -> Maybe ((Int, Int), String)
parsePos str
  | Just l <- readMaybe line
  , Just c <- readMaybe col  = Just ((l, c), rest)
  | otherwise                = Nothing
  where
    lparen        = dropWhile (/= '(') str
    (line, colon) = break (== ':') (drop 1 lparen)
    (col, rest)   = span (/= ')') (drop 1 colon)

-- | A logging level. At each logging level, only produce output at that level or lower.
data LogLevel
  = LogError
  -- ^ At level 'LogError', only error messages are shown.
  | LogWarn
  -- ^ At level 'LogWarn', error and warning messages are shown.
  | LogInfo
  -- ^ At level 'LogInfo', error, warning and information messages are shown.
  | LogDebug
  -- ^ At level 'LogDebug', error, warning, information and debug output is
  -- shown.
  deriving (Show, Eq, Ord)

instance Describe LogLevel where
  describeBuilder LogError = "ERROR"
  describeBuilder LogWarn  = "WARN"
  describeBuilder LogInfo  = "INFO"
  describeBuilder LogDebug = "DEBUG"

-- | A logged message with an origin and a message value.
data LogMessage a =
  LogMessage
  { _lmOrigin :: Maybe Origin
  , _lmMsg    :: a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

makeLenses ''LogMessage

instance NFData a => NFData (LogMessage a)

instance Describe a => Describe (LogMessage a) where
  describeBuilder msg =
    maybe "" describeBuilder (msg ^. lmOrigin) <>
    ": " <> describeBuilder (msg ^. lmMsg)

-- | A message at one of the four 'LogLevel's.
data SomeMessage e w
  = MsgError (LogMessage e)
  | MsgWarn (LogMessage w)
  | MsgInfo (LogMessage Text)
  | MsgDebug (LogMessage Text)
  deriving (Show, Eq, Generic)

makePrisms ''SomeMessage

instance (NFData e, NFData w) => NFData (SomeMessage e w)

someMessageOrigin :: Lens' (SomeMessage e w) (Maybe Origin)
someMessageOrigin =
  lens
  (preview $
    (_MsgError . lmOrigin `failing`
     _MsgWarn  . lmOrigin `failing`
     _MsgInfo  . lmOrigin `failing`
     _MsgDebug . lmOrigin
    ) . _Just)
  (flip $ \o ->
   set (_MsgError . lmOrigin) o .
   set (_MsgWarn  . lmOrigin) o .
   set (_MsgInfo  . lmOrigin) o .
   set (_MsgDebug . lmOrigin) o)


instance (Describe e, Describe w) => Describe (SomeMessage e w) where
  describeBuilder msg = case msg of
    MsgError m -> "ERROR: " <> describeBuilder m
    MsgWarn  m -> "WARN: "  <> describeBuilder m
    MsgInfo  m -> "INFO: "  <> describeBuilder m
    MsgDebug m -> "DEBUG: " <> describeBuilder m

--------------------------------------------------------------------------------
--  'MonadLogger' class
--------------------------------------------------------------------------------

-- | Make an origin at the source span of a piece of Fortran syntax, in the
-- current file.
atSpanned :: (MonadLogger e w m, F.Spanned a) => a -> m Origin
atSpanned astElem = do
  sf <- getDefaultSourceFile
  let sp = F.getSpan astElem
  return $ Origin sf sp

-- | Make an origin at the source span of a piece of Fortran syntax, in the given
-- file.
atSpannedInFile :: (F.Spanned a) => FilePath -> a -> Origin
atSpannedInFile sf = Origin sf . F.getSpan

-- | MTL-style type class for monads that support logging.
class Monad m => MonadLogger e w m | m -> e w where
  -- | Set the default source file, i.e. the file in which messages originate by
  -- default.
  setDefaultSourceFile :: FilePath -> m ()

  -- | Get the current default source file, i.e. the file in which messages
  -- originate by default.
  getDefaultSourceFile :: m FilePath

  -- | Record a log message. Output it based on the 'LogOutput' function used
  -- and store it in the collected logs.
  recordLogMessage :: SomeMessage e w -> m ()

  -- | Log an error message at the given 'Origin'.
  logError :: Origin -> e -> m ()
  logError = logGeneral MsgError

  -- | Log an error message. The origin is the current default source file, with
  -- the source span of the given piece of Fortran syntax.
  logError' :: (F.Spanned a) => a -> e -> m ()
  logError' = withSpannedOrigin logError


  -- | Log a warning message at the given 'Origin'.
  logWarn :: Origin -> w -> m ()
  logWarn = logGeneral MsgWarn

  -- | Log a warning message. The origin is the current default source file, with
  -- the source span of the given piece of Fortran syntax.
  logWarn' :: (F.Spanned a) => a -> w -> m ()
  logWarn' = withSpannedOrigin logWarn


  -- | Log an information message at the given 'Origin'.
  logInfo :: Origin -> Text -> m ()
  logInfo = logGeneral MsgInfo

  -- | Log an information message. The origin is the current default source
  -- file, with the source span of the given piece of Fortran syntax.
  logInfo' :: (F.Spanned a) => a -> Text -> m ()
  logInfo' = withSpannedOrigin logInfo

  -- | Log an information message with no origin. For example, use this when
  -- printing output about the progress of an analysis which cannot be
  -- associated with a particular bit of source code.
  logInfoNoOrigin :: Text -> m ()
  logInfoNoOrigin msg = recordLogMessage (MsgInfo (LogMessage Nothing msg))


  -- | Log a debugging message at the given 'Origin'.
  logDebug :: Origin -> Text -> m ()
  logDebug = logGeneral MsgDebug

  -- | Log a debugging message. The origin is the current default source
  -- file, with the source span of the given piece of Fortran syntax.
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

logGeneral :: (MonadLogger e w m) => (LogMessage a -> SomeMessage e w) -> Origin -> a -> m ()
logGeneral mkMsg origin msg =
  recordLogMessage (mkMsg (LogMessage (Just origin) msg))

withSpannedOrigin
  :: (MonadLogger e w m, F.Spanned a)
  => (Origin -> b -> m c) -> a -> b -> m c
withSpannedOrigin f x m = do
  origin <- atSpanned x
  f origin m

instance MonadLogger e w m => MonadLogger e w (ReaderT r m)
instance MonadLogger e w m => MonadLogger e w (ExceptT e' m)
instance MonadLogger e w m => MonadLogger e w (StateT s m)
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (WriterT w' m)
instance MonadLogger e w m => MonadLogger e w (Lazy.StateT s m)
-- instance (MonadLogger e w m, Monoid w') => MonadLogger e w (Lazy.WriterT w' m)
instance (MonadLogger e w m, Monoid w') => MonadLogger e w (RWST r w' s m)
-- instance (MonadLogger e w m, Monoid w') => MonadLogger e w (Lazy.RWST r w' s m)

--------------------------------------------------------------------------------
--  'LoggerT' monad
--------------------------------------------------------------------------------

data LoggerState =
  LoggerState
  { _lsLogLevel          :: !LogLevel
  , _lsDefaultSourceFile :: !FilePath
  , _lsPreviousOrigin    :: !(Maybe Origin)
  }

data OpMonoid a = OpMonoid { getOpMonoid :: a }

makeWrapped ''OpMonoid

instance SG.Semigroup a => SG.Semigroup (OpMonoid a) where
  OpMonoid x <> OpMonoid y = OpMonoid (y SG.<> x)

instance (SG.Semigroup a, Monoid a) => Monoid (OpMonoid a) where
  mempty = OpMonoid mempty
  mappend = (SG.<>)

data LoggerEnv m =
  LoggerEnv
  { _leLogFunc :: !(Bool -> LogLevel -> LogLevel -> Text -> Text -> m ())
  }

makeLenses ''LoggerState
makeLenses ''LoggerEnv

hoistEnv :: (m () -> n ()) -> LoggerEnv m -> LoggerEnv n
hoistEnv f = leLogFunc %~ \logFunc b l1 l2 m1 m2 -> f $ logFunc b l1 l2 m1 m2

-- | The logging monad transformer, containing errors of type @e@ and warnings
-- of type @w@.
newtype LoggerT e w m a =
  LoggerT (RWST (LoggerEnv m) (OpMonoid [SomeMessage e w]) LoggerState m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError e'
    , MonadFail
    )

instance MonadTrans (LoggerT e w) where
  lift = LoggerT . lift

instance (MonadState s m) => MonadState s (LoggerT e w m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (MonadReader r m) => MonadReader r (LoggerT e w m) where
  ask = lift ask
  local f (LoggerT (RWST k)) = LoggerT $ RWST $ \e -> local f . k e

instance (MonadWriter w' m) => MonadWriter w' (LoggerT e w m) where
  tell = lift . tell

  listen (LoggerT (RWST k)) = LoggerT $ RWST $ \e s -> do
    ((x, w, s'), w') <- listen (k e s)
    return ((x, w'), w, s')

  pass (LoggerT (RWST k)) = LoggerT $ RWST $ \e s ->
    pass $ (\((x, f), w, s') -> ((x, w, s'), f)) <$> k e s

instance (Monad m, Describe e, Describe w) =>
         MonadLogger e w (LoggerT e w m) where
  setDefaultSourceFile = LoggerT . (lsDefaultSourceFile .=)
  getDefaultSourceFile = LoggerT (use lsDefaultSourceFile)

  recordLogMessage msg = do
    LoggerT $ tell (OpMonoid [msg])
    logSomeMessage msg


-- | This doesn't behave quite as you may think. When a 'LoggerT' is hoisted,
-- the resulting 'LoggerT' cannot output as it goes. It still collects logs to
-- be inspected when it finishes.
instance MFunctor (LoggerT e w) where
  hoist f (LoggerT (RWST k)) = LoggerT $ RWST $ \e s ->
    let e' = hoistEnv (const (return ())) e
    in f (k e' s)


-- | A function to output logs in a particular monad @m@.
data LogOutput m = LogOutput
  { _loConciseOutput :: Bool
  , _loPrintFunc     :: Text -> m ()
  }


-- | Output logs to standard output (i.e. the console).
logOutputStd
  :: MonadIO m
  => Bool
  -- ^ If 'True', print more concise output when message origin is repeated.
  -> LogOutput m
logOutputStd b = LogOutput
  { _loConciseOutput = b
  , _loPrintFunc = liftIO . Text.putStrLn
  }


-- | Output no logs.
logOutputNone
  :: Monad m
  => Bool
  -- ^ If 'True', print more concise output when message origin is repeated.
  -> LogOutput m
logOutputNone b = LogOutput
  { _loConciseOutput = b
  , _loPrintFunc = const (return ())
  }


-- | Run the logging monad transformer. Returns the action's result value and a
-- list of logs which were collected as it ran.
runLoggerT
  :: (Monad m, Describe e, Describe w)
  => FilePath
  -- ^ The initial default source file. This is only used for displaying message
  -- origins.
  -> LogOutput m
  -- ^ The logging output function. E.g. 'logOutputStd' or 'logOutputNone'.
  -> LogLevel
  -- ^ The log level for on-the-fly logging. Doesn't affect which logs are
  -- collected at the end.
  -> LoggerT e w m a
  -- ^ The logging action to run.
  -> m (a, [SomeMessage e w])
runLoggerT sourceFile output logLevel (LoggerT action) = do
  let st = LoggerState
        { _lsLogLevel = logLevel
        , _lsDefaultSourceFile = sourceFile
        , _lsPreviousOrigin = Nothing
        }

      env = LoggerEnv
        { _leLogFunc = logFuncFrom output
        }

  (x, _, logs) <- runRWST action env st

  return (x, reverse (getOpMonoid logs))


-- | Change the error and warning types in a logger computation. To change the
-- underlying monad use 'hoist'.
mapLoggerT
  :: (Functor m)
  => (e -> e') -> (w -> w')
  -> LoggerT e w m a -> LoggerT e' w' m a
mapLoggerT mapErr mapWarn (LoggerT x) = LoggerT (mapRWST mapInner x)
  where
    mapInner =
      let messages ty = _3 . _Wrapped . traverse . ty . lmMsg
      in fmap (over (messages _MsgWarn) mapWarn . over (messages _MsgError) mapErr)

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

logFuncFrom
  :: (Monad m)
  => LogOutput m
  -> (Bool -> LogLevel -> LogLevel -> Text -> Text -> m ())
logFuncFrom LogOutput{ _loConciseOutput, _loPrintFunc } = lf
  where
    lf repeatedOrigin maxLevel level originMsg actualMsg
      | level <= maxLevel =
        let outputMsg =
              describeBuilder level <>
              (if not _loConciseOutput || not repeatedOrigin
               then " " <> describeBuilder originMsg else "") <> ": " <>
              describeBuilder actualMsg
        in _loPrintFunc (builderToStrict outputMsg)
      | otherwise = return ()


someLogLevel :: SomeMessage e w -> LogLevel
someLogLevel (MsgError _) = LogError
someLogLevel (MsgWarn _)  = LogWarn
someLogLevel (MsgInfo _)  = LogInfo
someLogLevel (MsgDebug _) = LogDebug

someMsgText :: (Describe e, Describe w) => SomeMessage e w -> Text
someMsgText (MsgError msg) = describe (msg ^. lmMsg)
someMsgText (MsgWarn msg)  = describe (msg ^. lmMsg)
someMsgText (MsgInfo msg)  = msg ^. lmMsg
someMsgText (MsgDebug msg) = msg ^. lmMsg


logSomeMessage
  :: (Monad m, Describe e, Describe w)
  => SomeMessage e w -> LoggerT e w m ()
logSomeMessage msg = do
  let msgText = someMsgText msg
      msgLevel = someLogLevel msg
      msgOrigin = msg ^. someMessageOrigin
      originText = maybe "" describe msgOrigin

  prevOrigin <- LoggerT $ use lsPreviousOrigin
  LoggerT $ lsPreviousOrigin .= msg ^. someMessageOrigin

  let repeatedOrigin = msgOrigin == prevOrigin

  logFunc <- LoggerT $ view leLogFunc
  logLevel <- LoggerT $ use lsLogLevel

  lift $ logFunc repeatedOrigin logLevel msgLevel originText msgText

formatError :: String -> String
formatError msg = bold (red msg)

formatSuccess :: String -> String
formatSuccess msg = bold (green msg)

bold :: String -> String
bold = txtColor "1"

-- black, red, green, yellow, blue, magenta, cyan, white :: String -> String
red, green :: String -> String
red = txtColor "31"
green = txtColor "32"

-- Not used currently but left here as a comment in case useful later

-- black = txtColor "30"
-- yellow = txtColor "33"
-- blue = txtColor "34"
-- magenta = txtColor "35"
-- cyan = txtColor "36"
-- white = txtColor "37"

txtColor :: String -> String -> String
txtColor colorCode message =
    -- if noColors
    --  then message
    "\ESC[" <> colorCode <> ";1m" <> message <> reset
  where
    reset = "\ESC[0m"