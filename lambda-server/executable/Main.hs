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
import ClassyPrelude hiding ((<>))
import Options.Applicative

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
main :: IO ()
main = execParser cmdParser >>= executeCmd

--------------------------------------------------------------------------------
executeCmd :: Run -> IO ()
executeCmd run = exec (runGeneralSettings run)
