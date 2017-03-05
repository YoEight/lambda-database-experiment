{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Server.Exec
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Exec (exec) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Protocol.Message
import Protocol.Operation
import Protocol.Package

--------------------------------------------------------------------------------
import Server.Bus
import Server.Connection
import Server.Messages
import Server.Messaging
import Server.Operation
import Server.QueuePublisher
import Server.Settings
import Server.Timer

--------------------------------------------------------------------------------
exec :: Settings -> IO ()
exec setts = do
  conn      <- newServerConnection $ connectionSettings setts
  mainBus   <- newBus "main-bus"
  mainQueue <- newQueuePublisher "main-queue" mainBus
  newOperationExec setts mainBus mainQueue

  forever $ do
    client <- awaitClientConnection conn
    env    <- newEnv setts client

    subscribe mainBus (onReceivePkg env mainQueue)
    subscribe mainBus (onHeartbeat env mainQueue)
    subscribe mainBus (onHeartbeatTimeout env mainQueue)
    subscribe mainBus (onShutdown env)

    scheduleHeartbeat env mainQueue

--------------------------------------------------------------------------------
onReceivePkg :: Publish pub => Env -> pub -> RecvPkg -> IO ()
onReceivePkg env@Env{..} pub (RecvPkg pkg) = do
  incrMsgNum env

  for_ (parseOp pkg) $ \op ->
    publish pub op

--------------------------------------------------------------------------------
onHeartbeat :: Publish pub => Env -> pub -> Heartbeat -> IO ()
onHeartbeat env@Env{..} pub (Heartbeat num) = do
  cur <- readIORef _msgNum

  if cur /= num
    then scheduleHeartbeat env pub
    else do
      pkg <- heartbeatRequest
      publish pub (TcpSend pkg)
      scheduleHeartbeatTimeout env pub

--------------------------------------------------------------------------------
onHeartbeatTimeout :: Publish pub => Env -> pub -> HeartbeatTimeout -> IO ()
onHeartbeatTimeout env@Env{..} pub (HeartbeatTimeout num) = do
  cur <- readIORef _msgNum

  when (cur == num) $
    terminate env

  scheduleHeartbeat env pub

--------------------------------------------------------------------------------
onShutdown :: Env -> Shutdown -> IO ()
onShutdown env Shutdown = terminate env

--------------------------------------------------------------------------------
data Env =
  Env { _conn     :: ClientConnection
      , _setts    :: Settings
      , _msgNum   :: IORef Integer
      }

--------------------------------------------------------------------------------
newEnv :: Settings -> ClientConnection -> IO Env
newEnv setts conn =
  Env conn setts <$> newIORef 0

--------------------------------------------------------------------------------
scheduleHeartbeat :: Publish pub => Env -> pub -> IO ()
scheduleHeartbeat Env{..} pub = do
  num <- readIORef _msgNum
  delayed (heartbeatInterval _setts) $
    publish pub (Heartbeat num)

--------------------------------------------------------------------------------
scheduleHeartbeatTimeout :: Publish pub => Env -> pub -> IO ()
scheduleHeartbeatTimeout Env{..} pub = do
  num <- readIORef _msgNum
  delayed (heartbeatTimeout _setts) $
    publish pub (HeartbeatTimeout num)

--------------------------------------------------------------------------------
incrMsgNum :: Env -> IO ()
incrMsgNum Env{..} = atomicModifyIORef' _msgNum $ \i -> (succ i, ())

--------------------------------------------------------------------------------
terminate :: Env -> IO ()
terminate Env{..} = do
  close _conn
  fail "terminate client connection"
