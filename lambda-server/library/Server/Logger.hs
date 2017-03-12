--------------------------------------------------------------------------------
-- |
-- Module : Server.Logger
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Logger
  ( LogManager
  , Logger
  , LogLevel(..)
  , newLogManager
  , getLogger
  , logMsg
  , logFormat
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Text.Format
import Data.Text.Format.Params
import System.Log.FastLogger

--------------------------------------------------------------------------------
import Server.Settings

--------------------------------------------------------------------------------
data LogManager =
  LogManager { logCallback :: TimedFastLogger }

--------------------------------------------------------------------------------
data Logger =
  Logger { loggerName     :: Text
         , loggerCallback :: TimedFastLogger
         }

--------------------------------------------------------------------------------
data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  | Fatal
  deriving (Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
logLvlTxt :: LogLevel -> Text
logLvlTxt Debug = "[DEBUG]"
logLvlTxt Info  = "[INFO]"
logLvlTxt Warn  = "[WARN]"
logLvlTxt Error = "[ERROR]"
logLvlTxt Fatal = "[FATAL]"

--------------------------------------------------------------------------------
newLogManager :: LoggerSettings -> IO LogManager
newLogManager setts = do
  cache         <- newTimeCache simpleTimeFormat'
  (callback, _) <- newTimedFastLogger cache (loggerType setts)
  return (LogManager callback)

--------------------------------------------------------------------------------
getLogger :: Text -> LogManager -> Logger
getLogger name mgr =
  Logger { loggerName     = name
         , loggerCallback = logCallback mgr
         }

--------------------------------------------------------------------------------
logMsg :: MonadIO m => Logger -> LogLevel -> Text -> m ()
logMsg Logger{..} lvl msg = liftIO $
  loggerCallback $ \t ->
    toLogStr t <> toLogStr (logLvlTxt lvl)
               <> toLogStr ("[" <> loggerName <> "]:")
               <> toLogStr msg

--------------------------------------------------------------------------------
logFormat :: (MonadIO m, Params ps)
          => Logger
          -> LogLevel
          -> Format
          -> ps
          -> m ()
logFormat Logger{..} lvl fm ps = liftIO $
  loggerCallback $ \t ->
    toLogStr t <> toLogStr (logLvlTxt lvl)
               <> toLogStr ("[" <> loggerName <> "]:")
               <> toLogStr (format fm ps)