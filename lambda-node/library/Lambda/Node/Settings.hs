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

--------------------------------------------------------------------------------
import Lambda.Node.Prelude

--------------------------------------------------------------------------------
data Settings =
  Settings
  { connectionSettings :: !ConnectionSettings }

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
parseSettings = Settings <$> parseConnectionSettings

--------------------------------------------------------------------------------
data ConnectionSettings =
  ConnectionSettings
  { portNumber :: !PortNumber
  , hostname   :: !String
  }

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