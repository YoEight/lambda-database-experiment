--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Logger
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Logger
  ( LoggerFilter(..)
  , LoggerRef(..)
  , LogType(..)
  , loggerCallback
  , toLogPredicate
  , newLoggerRef
  -- * Re-exports
  , module Control.Monad.Logger
  , module Control.Monad.Logger.CallStack
  , module Data.String.Interpolate.IsString
  ) where

--------------------------------------------------------------------------------
import Control.Monad

--------------------------------------------------------------------------------
import Control.Monad.Logger hiding (logDebug, logInfo, logWarn, logError, logOther, logWarnSH, logOtherSH, logDebugSH, logInfoSH, logErrorSH)
import Control.Monad.Logger.CallStack
import Data.String.Interpolate.IsString
import System.Log.FastLogger

--------------------------------------------------------------------------------
data LoggerFilter
  = LoggerFilter (LogSource -> LogLevel -> Bool)
  | LoggerLevel LogLevel

--------------------------------------------------------------------------------
toLogPredicate :: LoggerFilter -> (LogSource -> LogLevel -> Bool)
toLogPredicate (LoggerFilter k)  = k
toLogPredicate (LoggerLevel lvl) = \_ t -> t >= lvl

--------------------------------------------------------------------------------
data LoggerRef
  = LoggerRef !TimedFastLogger !LoggerFilter !Bool !(IO ())
  | NoLogger

--------------------------------------------------------------------------------
loggerCallback :: LoggerRef -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
loggerCallback NoLogger = \_ _ _ _ -> return ()
loggerCallback (LoggerRef logger filt detailed _) = \loc src lvl msg ->
  when (predicate src lvl) $
    loggerFormat logger (if detailed then loc else defaultLoc) src lvl msg
  where
    predicate = toLogPredicate filt

--------------------------------------------------------------------------------
loggerFormat :: TimedFastLogger
             -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
loggerFormat logger = \loc src lvl msg ->
  logger $ \t ->
    toLogStr ("["`mappend` t `mappend`"]") `mappend` " eventstore "
                                           `mappend` defaultLogStr loc src lvl msg

--------------------------------------------------------------------------------
newLoggerRef :: LogType -> LoggerFilter -> Bool -> IO LoggerRef
newLoggerRef LogNone _ _ = return NoLogger
newLoggerRef typ filt detailed =
  case typ of
    LogNone -> return NoLogger
    other   -> do
      cache             <- newTimeCache simpleTimeFormat
      (logger, cleanup) <- newTimedFastLogger cache other
      return $ LoggerRef logger filt detailed cleanup
