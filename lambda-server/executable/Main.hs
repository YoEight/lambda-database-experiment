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

--------------------------------------------------------------------------------
data Run =
  Run { runConnSettings :: ConnectionSettings }

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
parseRun = Run <$> parseConnectionSettings

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
main :: IO ()
main = execParser cmdParser >>= executeCmd

--------------------------------------------------------------------------------
executeCmd :: Run -> IO ()
executeCmd run = exec (runConnSettings run)
