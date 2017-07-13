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
module Lambda.Node.Manager.Connection where

--------------------------------------------------------------------------------
import Network.Simple.TCP

--------------------------------------------------------------------------------
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
createSocket :: ConnectionSettings -> IO ServerSocket
createSocket ConnectionSettings{..} = do
  (sock, addr) <- bindSock (Host hostname) (show portNumber)
  return $ ServerSocket sock addr

--------------------------------------------------------------------------------
acceptConnection :: ServerSocket -> IO ()
acceptConnection ServerSocket{..} =
  void $ acceptFork _serverSocket $ \(client, _) -> do
    return ()


--------------------------------------------------------------------------------
