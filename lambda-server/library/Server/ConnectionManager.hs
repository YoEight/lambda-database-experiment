--------------------------------------------------------------------------------
-- |
-- Module : Server.ConnectionManager
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.ConnectionManager where

--------------------------------------------------------------------------------
import Control.Monad.Fix

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Text.Format

--------------------------------------------------------------------------------
import Server.ClientManager
import Server.Bus
import Server.Connection
import Server.Messages
import Server.Messaging
import Server.MultiQueuePublisher
import Server.QueuePublisher
import Server.Settings
import Server.Types

--------------------------------------------------------------------------------
type Connections = HashMap ConnectionId ClientConnection

--------------------------------------------------------------------------------
data Runtime =
  Runtime { _runSettings  :: Settings
          , _runMainPub   :: SomePublisher
          , _runWorkerPub :: SomePublisher
          , _runConns     :: IORef Connections
          }

--------------------------------------------------------------------------------
connectionBusName :: ConnectionId -> Text
connectionBusName cid = "[" <> pack (show cid) <> "]"

--------------------------------------------------------------------------------
listeningWorker :: Runtime -> ServerConnection -> IO ()
listeningWorker Runtime{..} conn = forever $ do
  client <- awaitClientConnection conn
  publish _runMainPub (NewConnection client)

--------------------------------------------------------------------------------
onSystemInit :: Runtime -> SystemInit -> IO ()
onSystemInit run@Runtime{..} _ = do
  conn <- newServerConnection $ connectionSettings _runSettings
  _    <- forkFinally (listeningWorker run conn) $ \_ ->
            publish _runMainPub Shutdown

  publish _runMainPub (Initialized ConnectionService)

--------------------------------------------------------------------------------
onNewConnection :: Runtime -> NewConnection -> IO ()
onNewConnection Runtime{..} (NewConnection conn) = do
  atomicModifyIORef' _runConns $ \m ->
    (insertMap (connId conn) conn m, ())

  newClientManager _runSettings _runWorkerPub conn

--------------------------------------------------------------------------------
onConnectionClosed :: Runtime -> ConnectionClosed -> IO ()
onConnectionClosed Runtime{..} (ConnectionClosed cid) =
  atomicModifyIORef' _runConns $ \m ->
    (deleteMap cid m, ())

--------------------------------------------------------------------------------
onTcpSend :: Runtime -> TcpSend -> IO ()
onTcpSend Runtime{..} (TcpSend cid pkg) = do
  m <- readIORef _runConns
  for_ (lookup cid m) $ \conn -> do
    outcome <- tryAny $ send conn pkg
    case outcome of
      Left _  -> publish _runMainPub (ConnectionClosed cid)
      Right _ -> return ()

--------------------------------------------------------------------------------
connectionManager :: (Subscribe sub, Publish pub)
                  => Settings
                  -> sub
                  -> pub
                  -> IO ()
connectionManager setts mainSub mainPub = do
  count <- newIORef (0 :: Int)
  run   <- mfix $ \runtime -> do
    pubs <- replicateM 8 $ do
      idx <- atomicModifyIORef' count $ \i -> (succ i, i)

      let queueName = toStrict $ format "queue-{}" (Only idx)
          busName   = toStrict $ format "worker-{}" (Only idx)
      bus <- newBus busName

      subscribe bus (onTcpSend runtime)
      asPublisher <$> newQueuePublisher queueName bus

    workersQueue <- newMultiQueuePublisher "workers-queue" pubs
    let mQueue = asPublisher mainPub
        wQueue = asPublisher workersQueue

    Runtime setts mQueue wQueue <$> newIORef mempty

  subscribe_ mainSub (onNewConnection run)
  subscribe_ mainSub (onConnectionClosed run)
  subscribe_ mainSub (onSystemInit run)