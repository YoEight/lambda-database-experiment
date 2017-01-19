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
import Protocol.Package

--------------------------------------------------------------------------------
import Server.Connection
import Server.Settings
import Server.Timer

--------------------------------------------------------------------------------
exec :: Settings -> IO ()
exec setts = do
  conn <- newServerConnection $ connectionSettings setts
  forever $ do
    client <- awaitClientConnection conn
    env    <- newEnv setts client

    spawn env ReceiverActor
    spawn env WriterActor
    scheduleHeartbeat env

    fork $ exchange env

--------------------------------------------------------------------------------
data Env =
  Env { _conn     :: ClientConnection
      , _setts    :: Settings
      , _msgNum   :: IORef Integer
      , _msgQueue :: TQueue Msg
      , _pkgQueue :: TQueue Pkg
      }

--------------------------------------------------------------------------------
newEnv :: Settings -> ClientConnection -> IO Env
newEnv setts conn =
  Env conn setts <$> newIORef 0
                 <*> newTQueueIO
                 <*> newTQueueIO

--------------------------------------------------------------------------------
data Msg
  = Recv Pkg
  | Heartbeat Integer
  | HeartbeatTimeout Integer
  | Stop

--------------------------------------------------------------------------------
data ActorType = ReceiverActor | WriterActor

--------------------------------------------------------------------------------
receiver :: TQueue Msg -> ClientConnection -> IO ()
receiver msgQueue c = forever $ do
  pkg <- recv c
  atomically $ writeTQueue msgQueue (Recv pkg)

--------------------------------------------------------------------------------
writer :: TQueue Pkg -> ClientConnection -> IO ()
writer pkgQueue conn = forever $ do
  pkg <- atomically $ readTQueue pkgQueue
  send conn pkg

--------------------------------------------------------------------------------
spawn :: Env -> ActorType -> IO ()
spawn Env{..} tpe = do
  let action =
        case tpe of
          ReceiverActor -> receiver _msgQueue _conn
          WriterActor   -> writer _pkgQueue _conn

  _ <- forkFinally action $ \_ ->
         atomically $ writeTQueue _msgQueue Stop

  return ()

--------------------------------------------------------------------------------
scheduleHeartbeat :: Env -> IO ()
scheduleHeartbeat Env{..} = do
  num <- readIORef _msgNum
  delayed (heartbeatInterval _setts) $
    atomically $ writeTQueue _msgQueue (Heartbeat num)

--------------------------------------------------------------------------------
scheduleHeartbeatTimeout :: Env -> IO ()
scheduleHeartbeatTimeout Env{..} = do
  num <- readIORef _msgNum
  delayed (heartbeatTimeout _setts) $
    atomically $ writeTQueue _msgQueue (HeartbeatTimeout num)

--------------------------------------------------------------------------------
incrMsgNum :: Env -> IO ()
incrMsgNum Env{..} = atomicModifyIORef' _msgNum $ \i -> (succ 1, ())

--------------------------------------------------------------------------------
exchange :: Env -> IO ()
exchange env@Env{..} = forever $ do
  msg <- atomically $ readTQueue _msgQueue
  case msg of
    Recv pkg -> do
      incrMsgNum env

    Heartbeat num -> do
      cur <- readIORef _msgNum

      if cur /= num
        then scheduleHeartbeat env
        else do
          pkg <- heartbeatRequest
          atomically $ writeTQueue _pkgQueue pkg
          scheduleHeartbeatTimeout env

    HeartbeatTimeout num -> do
      cur <- readIORef _msgNum

      when (cur == num) $
        terminate env

      scheduleHeartbeat env

    Stop -> terminate env

--------------------------------------------------------------------------------
terminate :: Env -> IO ()
terminate Env{..} = do
  close _conn
  fail "terminate client connection"
