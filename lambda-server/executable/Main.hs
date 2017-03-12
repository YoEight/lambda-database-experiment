{-# LANGUAGE StrictData   #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------------------
-- |
-- Module : Main
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import ClassyPrelude
import Options.Applicative
import System.Log.FastLogger hiding (check)

--------------------------------------------------------------------------------
import Server.Connection
import Server.Exec
import Server.Settings
import Server.Timer

--------------------------------------------------------------------------------
data Run =
  Run { runGeneralSettings :: Settings }

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------
cmdParser :: ParserInfo Run
cmdParser = info (helper <*> parseRun) description
  where
    description =
      fullDesc <> header "LDE - Lambda Database Experiment."
               <> progDesc "Starts the LDE server."

--------------------------------------------------------------------------------
parseRun :: Parser Run
parseRun = Run <$> parseGeneralSettings

--------------------------------------------------------------------------------
parseGeneralSettings :: Parser Settings
parseGeneralSettings =
  Settings <$> parseConnectionSettings
           <*> parseHeartbeatInterval
           <*> parseHeartbeatTimeout
           <*> parseDbFile
           <*> parseLoggerSettings

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
        Just port -> Right port

--------------------------------------------------------------------------------
parseHeartbeatInterval :: Parser Duration
parseHeartbeatInterval = option (eitherReader durationReader) go
  where
    go = long "heartbeat-interval"
           <> metavar "HEARTBEAT_INTERVAL"
           <> help "Heartbeat interval (ms)"
           <> value (msDuration 2000)
           <> showDefault

--------------------------------------------------------------------------------
parseHeartbeatTimeout :: Parser Duration
parseHeartbeatTimeout = option (eitherReader durationReader) go
  where
    go = long "heartbeat-timeout"
           <> metavar "HEARTBEAT_TIMEOUT"
           <> help "Heartbeat timeout (ms)"
           <> value (msDuration 1000)
           <> showDefault

--------------------------------------------------------------------------------
durationReader :: String -> Either String Duration
durationReader input =
  case readMay input of
    Nothing -> Left "Invalid duration"
    Just ms -> Right $ msDuration ms

--------------------------------------------------------------------------------
parseDbFile :: Parser String
parseDbFile = strOption go
  where
    go = long "db" <> metavar "DB_FILE"
                   <> help "Database file"
                   <> value "store.db"
                   <> showDefault

--------------------------------------------------------------------------------
parseLoggerSettings :: Parser LoggerSettings
parseLoggerSettings = go <$> parseLogOutput <*> parseLogBufSize
  where
    go tpe bufSize =
      let logType =
            case strToLogType tpe of
              LogStdout _            -> LogStdout bufSize
              LogStderr _            -> LogStderr bufSize
              LogFileNoRotate path _ -> LogFileNoRotate path bufSize
              LogFile spec _         -> LogFile spec bufSize
              l                      -> l in
      LoggerSettings logType

--------------------------------------------------------------------------------
parseLogOutput :: Parser String
parseLogOutput = strOption go
  where
    go = long "log-output" <> metavar "LOG_OUTPUT"
                           <> help "Output of logging"
                           <> value "stdout"
                           <> showDefault

--------------------------------------------------------------------------------
parseLogBufSize :: Parser Int
parseLogBufSize = option (eitherReader check) go
  where
    go = long "log-buf-size" <> metavar "LOG_BUF_SIZE"
                             <> help "Logging buffer size"
                             <> value 0
                             <> showDefault
    check str =
      case readMay str of
        Just i -> Right i
        _      -> Left $ "Logging buffer size should be an integer ["
                       <> str <> "]"

--------------------------------------------------------------------------------
strToLogType :: String -> LogType
strToLogType "stdout" = LogStdout 0
strToLogType "stderr" = LogStderr 0
strToLogType path     = LogFileNoRotate path 0

--------------------------------------------------------------------------------
main :: IO ()
main = execParser cmdParser >>= executeCmd

--------------------------------------------------------------------------------
executeCmd :: Run -> IO ()
executeCmd run = exec (runGeneralSettings run)
