--------------------------------------------------------------------------------
-- |
-- Module : Server.ClientManager
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.ClientManager (newClientManager) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Protocol.Message
import Protocol.Package

--------------------------------------------------------------------------------
import Server.Bus
import Server.Connection
import Server.Messages
import Server.Messaging
import Server.QueuePublisher
import Server.Settings
import Server.Timer

--------------------------------------------------------------------------------
data ClientEnv =
  ClientEnv { _setts   :: Settings
            , _conn    :: ClientConnection
            , _mainPub :: SomePublisher
            , _msgNum  :: IORef Integer
            }

--------------------------------------------------------------------------------
busName :: ClientConnection -> Text
busName c = "connection-alive-" <> pack (show (connId c))

--------------------------------------------------------------------------------
queueName :: ClientConnection -> Text
queueName q = "queue-" <> busName q

--------------------------------------------------------------------------------
newClientEnv :: Settings -> ClientConnection -> SomePublisher -> IO ClientEnv
newClientEnv setts conn pub =
  ClientEnv setts conn pub <$> newIORef 0

--------------------------------------------------------------------------------
newClientManager :: Settings -> SomePublisher -> ClientConnection -> IO ()
newClientManager setts pub conn = do
  env   <- newClientEnv setts conn (asPublisher pub)
  bus   <- newBus (busName conn)
  queue <- newQueuePublisher (queueName conn) bus

  let localPub = asPublisher queue
  _ <- forkFinally (clientWorker env) $ \_ ->
         publish pub (ConnectionClosed (connId conn))

  subscribe_ bus (onHeartbeat env localPub)
  subscribe_ bus (onHeartbeatTimeout env localPub)

--------------------------------------------------------------------------------
onHeartbeat :: ClientEnv -> SomePublisher -> Heartbeat -> IO ()
onHeartbeat env@ClientEnv{..} localPub (Heartbeat num) = do
  cur <- readIORef _msgNum

  if cur /= num
    then scheduleHeartbeat env localPub
    else do
      pkg <- heartbeatRequest
      publish _mainPub (TcpSend (connId _conn) pkg)
      scheduleHeartbeatTimeout env localPub

--------------------------------------------------------------------------------
onHeartbeatTimeout :: ClientEnv -> SomePublisher -> HeartbeatTimeout -> IO ()
onHeartbeatTimeout env@ClientEnv{..} localPub (HeartbeatTimeout num) = do
  cur <- readIORef _msgNum

  if cur == num
    then publish _mainPub (ConnectionClosed (connId _conn))
    else scheduleHeartbeat env localPub

--------------------------------------------------------------------------------
scheduleHeartbeat :: ClientEnv -> SomePublisher -> IO ()
scheduleHeartbeat ClientEnv{..} localPub = do
  num <- readIORef _msgNum
  delayed (heartbeatInterval _setts) $
    publish localPub (Heartbeat num)

--------------------------------------------------------------------------------
scheduleHeartbeatTimeout :: ClientEnv -> SomePublisher -> IO ()
scheduleHeartbeatTimeout ClientEnv{..} localPub = do
  num <- readIORef _msgNum
  delayed (heartbeatTimeout _setts) $
    publish localPub (HeartbeatTimeout num)

--------------------------------------------------------------------------------
incrMsgNum :: ClientEnv -> IO ()
incrMsgNum ClientEnv{..} = atomicModifyIORef' _msgNum $ \i -> (succ i, ())

--------------------------------------------------------------------------------
clientWorker :: ClientEnv -> IO ()
clientWorker env@ClientEnv{..} = forever $ do
  pkg <- recv _conn
  incrMsgNum env

  for_ (parseOp pkg) $ \op ->
    publish _mainPub (NewOperation (connId _conn) op)