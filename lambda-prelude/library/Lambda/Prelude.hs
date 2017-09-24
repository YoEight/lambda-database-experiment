{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Prelude
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Prelude
  ( module ClassyPrelude
  , module Data.String.Interpolate.IsString
  , module System.Clock
  , module Control.Monad.Fix
  , module Lambda.Logger
  -- * Lambda
  , Lambda(..)
  , getSettings
  , lambdaMain
  , lambdaMain_
  -- * Settings
  , PrettyPrint(..)
  , AppSettings(..)
  -- * Misc
  , UUID
  , NominalDiffTime
  , diffUTCTime
  , clockTime
  , freshUUID
  , s2ns
  , s2mcs
  ) where

--------------------------------------------------------------------------------
import Control.Monad.Fix
import Data.Proxy

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.Reader
import Data.Time (NominalDiffTime, diffUTCTime)
import Data.UUID
import Data.UUID.V4
import Data.String.Interpolate.IsString
import Lambda.Logger
import Options.Applicative
import System.Clock
import Text.PrettyPrint hiding ((<>))

--------------------------------------------------------------------------------
clockTime :: MonadIO m => m TimeSpec
clockTime = liftIO $ getTime Monotonic

--------------------------------------------------------------------------------
freshUUID :: MonadIO m => m UUID
freshUUID = liftIO nextRandom

--------------------------------------------------------------------------------
s2ns :: Num a => a
s2ns = 10^(9 :: Int)

--------------------------------------------------------------------------------
s2mcs :: Num a => a
s2mcs = 10^(6 :: Int)

--------------------------------------------------------------------------------
data Env app =
  Env { _settings  :: !(LambdaSettings app)
      , _loggerRef :: !LoggerRef
      }

--------------------------------------------------------------------------------
instance Functor Env where
  fmap f e = e { _settings = fmap f (_settings e) }

--------------------------------------------------------------------------------
class PrettyPrint p where
  pprint :: p -> Doc

--------------------------------------------------------------------------------
instance PrettyPrint () where
  pprint _ = mempty

--------------------------------------------------------------------------------
class PrettyPrint s => AppSettings s where
  settingsParser :: Parser s
  description    :: Proxy s -> InfoMod a

--------------------------------------------------------------------------------
instance AppSettings () where
  settingsParser = pure ()
  description _ = mempty

--------------------------------------------------------------------------------
data LambdaSettings settings =
  LambdaSettings { loggingSettings :: !LoggingSettings
                 , appSettings     :: !settings
                 }

--------------------------------------------------------------------------------
instance Functor LambdaSettings where
  fmap f s = s { appSettings = f (appSettings s) }

--------------------------------------------------------------------------------
instance PrettyPrint s => PrettyPrint (LambdaSettings s) where
  pprint LambdaSettings{..} =
    vcat [ "Settings:"
         , nest 5 $
           vcat [ "Logging Settings:"
                , nest 5 (ppLoggingSettings loggingSettings)
                , "Application Settings:"
                , nest 5 (pprint appSettings)
                ]
         ]

--------------------------------------------------------------------------------
applyProxy :: Proxy (LambdaSettings s) -> Proxy s
applyProxy _ = Proxy

--------------------------------------------------------------------------------
instance AppSettings s => AppSettings (LambdaSettings s) where
  settingsParser = parseLambdaSettings
  description prx = description (applyProxy prx)

--------------------------------------------------------------------------------
parseLambdaSettings :: AppSettings s => Parser (LambdaSettings s)
parseLambdaSettings =
  LambdaSettings <$> parseLoggingSettings
                 <*> settingsParser

--------------------------------------------------------------------------------
-- Main Lambda monad stack.
newtype Lambda settings a =
  Lambda { unLambda :: ReaderT (Env settings) IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadFix
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadBase IO
           , MonadBaseControl IO
           )

--------------------------------------------------------------------------------
data LoggingSettings =
  LoggingSettings
  { loggingType  :: !LogType
  , loggingLevel :: !LoggerFilter
  }

--------------------------------------------------------------------------------
ppLoggingSettings :: LoggingSettings -> Doc
ppLoggingSettings LoggingSettings{..} =
  vcat [ text "logging-type:"  <+> ppLogType loggingType
       , text "logging-level:" <+> ppLogFilter loggingLevel
       ]

--------------------------------------------------------------------------------
ppLogType :: LogType -> Doc
ppLogType LogStdout{}              = text "stdout"
ppLogType LogStderr{}              = text "stderr"
ppLogType (LogFileNoRotate path _) = text path
ppLogType _                        = text "*not supported*"

--------------------------------------------------------------------------------
ppLogFilter :: LoggerFilter -> Doc
ppLogFilter (LoggerLevel lvl) = ppLogLevel lvl
ppLogFilter _                 = text "*not supported*"

--------------------------------------------------------------------------------
ppLogLevel :: LogLevel -> Doc
ppLogLevel LevelDebug = text "debug"
ppLogLevel LevelInfo  = text "info"
ppLogLevel LevelWarn  = text "warn"
ppLogLevel LevelError = text "error"
ppLogLevel _          = text "*not supported*"

--------------------------------------------------------------------------------
parseLoggingSettings :: Parser LoggingSettings
parseLoggingSettings =
  LoggingSettings <$> parseLoggingType
                  <*> parseLoggingLevel

--------------------------------------------------------------------------------
parseLoggingType :: Parser LogType
parseLoggingType = to <$> strOption go
  where
    go = long "logging-type" <> metavar "LOGGING_TYPE"
                             <> help "Logging type: stdout, stderr or a file path"
                             <> value "stdout"
                             <> showDefault

    to "stdout" = LogStdout 0
    to "stderr" = LogStderr 0
    to filepath = LogFileNoRotate filepath 0
--------------------------------------------------------------------------------
parseLoggingLevel :: Parser LoggerFilter
parseLoggingLevel = (LoggerLevel . to) <$> strOption go
  where
    go = long "logging-level" <> metavar "LOGGING_LEVEL"
                              <> help "Logging level: debug, info, warn and error"
                              <> value "info"
                              <> showDefault

    to :: String -> LogLevel
    to "debug" = LevelDebug
    to "info"  = LevelInfo
    to "warn"  = LevelWarn
    to "error" = LevelError
    to _       = LevelInfo

--------------------------------------------------------------------------------
getSettings :: Lambda settings settings
getSettings = Lambda (fmap (appSettings . _settings) ask)

--------------------------------------------------------------------------------
instance MonadLogger (Lambda settings) where
  monadLoggerLog loc src lvl msg = Lambda $
    do ref <- _loggerRef <$> ask
       liftIO $ loggerCallback ref loc src lvl (toLogStr msg)

--------------------------------------------------------------------------------
instance MonadReader settings (Lambda settings) where
  ask = Lambda (fmap (appSettings . _settings) ask)
  local k (Lambda m) = Lambda (local (fmap k) m)

--------------------------------------------------------------------------------
lambdaSettingsParser :: forall s. AppSettings s => ParserInfo (LambdaSettings s)
lambdaSettingsParser = info (helper <*> settingsParser) desc
  where
    desc = description (Proxy :: Proxy (LambdaSettings s))

--------------------------------------------------------------------------------
lambdaMain :: AppSettings settings => Lambda settings a -> IO a
lambdaMain (Lambda m) =
  do setts <- execParser lambdaSettingsParser
     putStrLn $ pack $ render $ pprint setts

     let logging  = loggingSettings setts
         logType  = loggingType logging
         logLevel = loggingLevel logging
         detailed = True

     env <- Env setts <$> newLoggerRef logType logLevel detailed
     runReaderT m env

--------------------------------------------------------------------------------
lambdaMain_ :: settings -> Lambda settings a -> IO a
lambdaMain_ appSetts (Lambda m) =
  do setts <- execParser lambdaSettingsParser
     let rightSetts = fmap (\() -> appSetts) setts
     putStrLn $ pack $ render $ pprint setts

     let logging  = loggingSettings setts
         logType  = loggingType logging
         logLevel = loggingLevel logging
         detailed = True

     env <- Env rightSetts <$> newLoggerRef logType logLevel detailed
     runReaderT m env
