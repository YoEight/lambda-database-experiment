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
  { _clientId     :: !UUID
  , _clientSocket :: !Socket
  , _clientAddr   :: !SockAddr
  , _clientBus    :: !Bus
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
acceptConnection :: Internal -> ServerSocket -> IO ()
acceptConnection Internal{..} ServerSocket{..} =
  void $ acceptFork _serverSocket $ \(sock, addr) -> do
    uuid <- freshUUID
    bus  <- newBus _runtime [i|bus-client-#{uuid}|]
    let client = ClientSocket uuid sock addr bus
    atomicModifyIORef' _connections $ \m -> (insertMap uuid client m, ())
    return ()

--------------------------------------------------------------------------------
listeningFork :: Internal -> ConnectionSettings -> IO ()
listeningFork self setts = do
  sock <- createSocket setts
  let go = acceptConnection self sock >> go
  void $ fork go


