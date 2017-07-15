--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Node.Manager.Connection
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Manager.Connection (new) where

--------------------------------------------------------------------------------
import Network.Simple.TCP

--------------------------------------------------------------------------------
import Data.Serialize
import Protocol.Package

--------------------------------------------------------------------------------
import Lambda.Node.Bus
import Lambda.Node.Logger
import Lambda.Node.Monitoring
import Lambda.Node.Prelude
import Lambda.Node.Settings
import Lambda.Node.Types

--------------------------------------------------------------------------------
data ServerSocket =
  ServerSocket
  { _serverSocket :: !Socket
  , _serverAddr   :: !SockAddr
  }

--------------------------------------------------------------------------------
data ClientSocket =
  ClientSocket
  { _clientSocket :: !Socket
  , _clientAddr   :: !SockAddr
  , _clientId     :: !UUID
  , _clientBus    :: !Bus
  , _clientRecv   :: !(Async ())
  , _clientSend   :: !(Async ())
  }

--------------------------------------------------------------------------------
type Connections = HashMap UUID ClientSocket

--------------------------------------------------------------------------------
data Internal =
  Internal
  { _runtime :: Runtime
  , _connections :: IORef Connections
  }

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------
newtype PackageArrived = PackageArrived Pkg

--------------------------------------------------------------------------------
new :: Settings -> IO ()
new setts = do
  runtime <- Runtime setts <$> newLoggerRef (LogStdout 0) (LoggerLevel LevelInfo) True
                           <*> createMonitoring

  self <- Internal runtime <$> newIORef mempty
  listeningFork self (connectionSettings setts)

--------------------------------------------------------------------------------
createSocket :: ConnectionSettings -> IO ServerSocket
createSocket ConnectionSettings{..} = do
  (sock, addr) <- bindSock (Host hostname) (show portNumber)
  return $ ServerSocket sock addr

--------------------------------------------------------------------------------
-- | TODO - Makes sure to cleanup everything in case of exception.
acceptConnection :: Internal -> ServerSocket -> IO ()
acceptConnection Internal{..} ServerSocket{..} =
  void $ acceptFork _serverSocket $ \(sock, addr) -> do
    client <- mfix $ \self ->
      ClientSocket sock addr <$> freshUUID
                             <*> newBus _runtime [i|bus-client-#{_clientId self}|]
                             <*> async (processingIncomingPackage self)
                             <*> async (processingOutgoingPackage self)

    atomicModifyIORef' _connections $ \m ->
      (insertMap (_clientId client) client m, ())
    busProcessedEverything $ _clientBus client

--------------------------------------------------------------------------------
listeningFork :: Internal -> ConnectionSettings -> IO ()
listeningFork self setts = do
  sock <- createSocket setts
  let go = acceptConnection self sock >> go
  void $ fork go

--------------------------------------------------------------------------------
processingIncomingPackage :: ClientSocket -> IO ()
processingIncomingPackage self@ClientSocket{..} = forever $ do
  prefixBytes <- recvExact self 4
  case decode prefixBytes of
    Left _    -> throwString "Wrong package framing."
    Right len -> do
      payload <- recvExact self len
      case decode payload of
        Left e    -> throwString [i|Package parsing error #{e}.|]
        Right pkg -> publishWith _clientBus (PackageArrived pkg)

--------------------------------------------------------------------------------
recvExact :: ClientSocket -> Int -> IO ByteString
recvExact = undefined

--------------------------------------------------------------------------------
processingOutgoingPackage :: ClientSocket -> IO ()
processingOutgoingPackage ClientSocket{..} = return ()