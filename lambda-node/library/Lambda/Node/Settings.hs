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
import Lambda.Logger
import Lambda.Prelude
import Options.Applicative
import Network
import Text.PrettyPrint hiding ((<>))

--------------------------------------------------------------------------------
data Settings =
  Settings
  { heartbeatInterval  :: !NominalDiffTime
  , heartbeatTimeout   :: !NominalDiffTime
  , connectionSettings :: !ConnectionSettings
  }

--------------------------------------------------------------------------------
instance PrettyPrint Settings where
  pprint Settings{..} =
    vcat [ text "heartbeat-interval: " <+> text (show heartbeatInterval)
         , text "heartbeat-timeout:"   <+> text (show heartbeatTimeout)
         , text "Connection settings:"
         , nest 5 (ppConnectionSettings connectionSettings)
         ]

--------------------------------------------------------------------------------
instance AppSettings Settings where
  settingsParser = parseSettings

  description _ =
    fullDesc <> header "LDE - Lambda Database Experiment."
             <> progDesc "Starts the LDE server."

--------------------------------------------------------------------------------
parseSettings :: Parser Settings
parseSettings = Settings <$> parseHeartbeatInterval
                         <*> parseHeartbeatTimeout
                         <*> parseConnectionSettings

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
