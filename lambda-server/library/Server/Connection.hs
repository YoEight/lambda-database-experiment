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
module Server.Connection
  ( ConnectionSettings(..)
  , ServerConnection
  , ConnectionException(..)
  , ClientId
  , ClientConnection
  , PortNumber
  , newServerConnection
  , awaitClientConnection
  , recv
  , send
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Serialize
import Data.UUID
import Data.UUID.V4
import Network
import Network.Connection

--------------------------------------------------------------------------------
import Protocol.Package

--------------------------------------------------------------------------------
data ConnectionException
  = MaxAttemptConnectionReached
  | WrongPackageFraming
  | PackageParsingError Text
  deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception ConnectionException

--------------------------------------------------------------------------------
data ConnectionSettings =
  ConnectionSettings { portNumber :: PortNumber
                     , hostname   :: String
                     }

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
    params = ConnectionParams { connectionHostname  = hostname connSettings
                              , connectionPort      = portNumber connSettings
                              , connectionUseSecure = Nothing
                              , connectionUseSocks  = Nothing
                              }

--------------------------------------------------------------------------------
recv :: ClientConnection -> IO Pkg
recv ClientConnection{..} = do
  prefixBytes <- connectionGetExact innerConn 4
  case decode prefixBytes of
    Left _    -> throwIO WrongPackageFraming
    Right len -> do
      payload <- connectionGetExact innerConn len
      case decode payload of
        Left e    -> throwIO $ PackageParsingError $ pack e
        Right pkg -> return pkg

--------------------------------------------------------------------------------
send :: ClientConnection -> Pkg -> IO ()
send ClientConnection{..} pkg = connectionPut innerConn $ encode pkg
