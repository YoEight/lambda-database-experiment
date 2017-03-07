{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , ClientConnection(..)
  , ConnectionClosed(..)
  , NewConnection(..)
  , PortNumber
  , newServerConnection
  , awaitClientConnection
  , recv
  , send
  , close
  ) where

--------------------------------------------------------------------------------
import           ClassyPrelude
import qualified Data.ByteString as B
import           Data.Serialize
import           Network

--------------------------------------------------------------------------------
import Protocol.Package
import Protocol.Types

--------------------------------------------------------------------------------
import Server.Types

--------------------------------------------------------------------------------
data ConnectionClosed = ConnectionClosed ConnectionId

--------------------------------------------------------------------------------
data NewConnection = NewConnection ClientConnection

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
                   , sock         :: Socket
                   }

--------------------------------------------------------------------------------
newServerConnection :: ConnectionSettings -> IO ServerConnection
newServerConnection cs@ConnectionSettings{..} =
  withSocketsDo (ServerConnection cs <$> listenOn (PortNumber portNumber))

--------------------------------------------------------------------------------
data ClientConnection =
  ClientConnection { connId    :: ConnectionId
                   , innerConn :: Handle
                   }

--------------------------------------------------------------------------------
awaitClientConnection :: ServerConnection -> IO ClientConnection
awaitClientConnection ServerConnection{..} =
  ClientConnection <$> freshId
                   <*> getHandle
  where
    getHandle = do
      (sockHandle, _, _) <- accept sock
      return sockHandle

--------------------------------------------------------------------------------
recv :: ClientConnection -> IO Pkg
recv ClientConnection{..} = do
  prefixBytes <- B.hGet innerConn 4
  case decode prefixBytes of
    Left _    -> throwIO WrongPackageFraming
    Right len -> do
      payload <- B.hGet innerConn len
      case decode payload of
        Left e    -> throwIO $ PackageParsingError $ pack e
        Right pkg -> return pkg

--------------------------------------------------------------------------------
send :: ClientConnection -> Pkg -> IO ()
send ClientConnection{..} pkg = B.hPut innerConn $ encode pkg

--------------------------------------------------------------------------------
close :: ClientConnection -> IO ()
close ClientConnection{..} = do
  (_ :: Either SomeException ()) <- try (hClose innerConn)
  return ()
