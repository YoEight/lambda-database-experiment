--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Node.Settings
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Settings where

--------------------------------------------------------------------------------
import Options.Applicative
import Network
import Text.PrettyPrint hiding ((<>))

--------------------------------------------------------------------------------
import Lambda.Node.Logger
import Lambda.Node.Prelude

--------------------------------------------------------------------------------
data Settings =
  Settings
  { heartbeatInterval  :: !NominalDiffTime
  , heartbeatTimeout   :: !NominalDiffTime
  , connectionSettings :: !ConnectionSettings
  , loggingSettings    :: !LoggingSettings
  }

--------------------------------------------------------------------------------
instance Show Settings where
  show = render . ppSettings

--------------------------------------------------------------------------------
ppSettings :: Settings -> Doc
ppSettings Settings{..} =
  vcat [ "Settings:"
       , nest 5 $
           vcat [ text "heartbeat-interval: " <+> text (show heartbeatInterval)
                , text "heartbeat-timeout:"   <+> text (show heartbeatTimeout)
                , text "Connection settings:"
                , nest 5 (ppConnectionSettings connectionSettings)
                , text "Logging Settings:"
                , nest 5 (ppLoggingSettings loggingSettings)
                ]
       ]

--------------------------------------------------------------------------------
parseArgs :: IO Settings
parseArgs = execParser settingsParser

--------------------------------------------------------------------------------
settingsParser :: ParserInfo Settings
settingsParser = info (helper <*> parseSettings) description
  where
    description =
      fullDesc <> header "LDE - Lambda Database Experiment."
               <> progDesc "Starts the LDE server."

--------------------------------------------------------------------------------
parseSettings :: Parser Settings
parseSettings = Settings <$> parseHeartbeatInterval
                         <*> parseHeartbeatTimeout
                         <*> parseConnectionSettings
                         <*> parseLoggingSettings

--------------------------------------------------------------------------------
parseHeartbeatInterval :: Parser NominalDiffTime
parseHeartbeatInterval = option (maybeReader check) go
  where
    go = long "heartbeat-interval" <> metavar "HEARTBEAT_INTERVAL"
                                   <> help "Heartbeat interval: Delay in which \
                                           \the server start to worry if it \
                                           \has no news from the client."
                                   <> value 0.5
                                   <> showDefault
    check input =
      fmap realToFrac (readMay input :: Maybe Double)

--------------------------------------------------------------------------------
parseHeartbeatTimeout :: Parser NominalDiffTime
parseHeartbeatTimeout = option (maybeReader check) go
  where
    go = long "heartbeat-timeout" <> metavar "HEARTBEAT_TIMEOUT"
                                  <> help "Heartbeat timeout: Delay that a \
                                           \client has to send a heartbeat \
                                           \response."
                                  <> value 0.75
                                  <> showDefault
    check input =
      fmap realToFrac (readMay input :: Maybe Double)

--------------------------------------------------------------------------------
data ConnectionSettings =
  ConnectionSettings
  { portNumber :: !PortNumber
  , hostname   :: !String
  }

--------------------------------------------------------------------------------
ppConnectionSettings :: ConnectionSettings -> Doc
ppConnectionSettings ConnectionSettings{..} =
  vcat [ text "host:" <+> text hostname
       , text "port:" <+> text (show portNumber)
       ]

--------------------------------------------------------------------------------
parseConnectionSettings :: Parser ConnectionSettings
parseConnectionSettings =
  ConnectionSettings <$> parsePort
                     <*> parseHost

--------------------------------------------------------------------------------
parseHost :: Parser String
parseHost = strOption go
  where
    go = long "host" <> metavar "HOST"
                     <> help "Server hostname address."
                     <> value "127.0.0.1"
                     <> showDefault

--------------------------------------------------------------------------------
parsePort :: Parser PortNumber
parsePort = option (eitherReader check) go
  where
    go = long "port" <> metavar "PORT"
                     <> help "Server port."
                     <> value 1113
                     <> showDefault

    check input =
      case readMay input of
        Nothing   -> Left "Invalid port number."
        Just port
           | port > 0 && port < 65535 -> Right port
           | otherwise -> Left [i|Port should be ]0-65535[|]

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

    to "debug" = LevelDebug
    to "info"  = LevelInfo
    to "warn"  = LevelWarn
    to "error" = LevelError
