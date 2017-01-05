{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Server.Connection
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Connection where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID
import Data.UUID.V4
import Network
import Network.Connection

--------------------------------------------------------------------------------
data ConnectionSettings =
  ConnectionSettings { portNumber :: PortNumber }

--------------------------------------------------------------------------------
data ServerConnection =
  ServerConnection { connSettings :: ConnectionSettings
                   , serverCtx    :: ConnectionContext
                   , sock         :: Socket
                   }

--------------------------------------------------------------------------------
newServerConnection :: ConnectionSettings -> IO ServerConnection
newServerConnection cs@ConnectionSettings{..} =
  ServerConnection cs <$> initConnectionContext
                      <*> listenOn (PortNumber portNumber)

--------------------------------------------------------------------------------
newtype ClientId = ClientId UUID deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
freshClientId :: IO ClientId
freshClientId = ClientId <$> nextRandom

--------------------------------------------------------------------------------
data ClientConnection =
  ClientConnection { clientId :: ClientId
                   , innerConn :: Connection
                   }

--------------------------------------------------------------------------------
awaitClientConnection :: ServerConnection -> IO ClientConnection
awaitClientConnection ServerConnection{..} =
  ClientConnection <$> freshClientId
                   <*> connectFromSocket serverCtx sock params
  where
    params = ConnectionParams { connectionHostname  = "localhost"
                              , connectionPort      = portNumber connSettings
                              , connectionUseSecure = Nothing
                              , connectionUseSocks  = Nothing
                              }
