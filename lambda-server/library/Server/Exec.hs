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
import Server.Connection
import Server.Operation
import Server.Settings
import Server.Timer

--------------------------------------------------------------------------------
exec :: Settings -> IO ()
exec setts = do
  conn   <- newServerConnection $ connectionSettings setts
  opExec <- newOperationExec setts
  forever $ do
    client <- awaitClientConnection conn
    env    <- newEnv setts opExec client

    spawn env ReceiverActor
    spawn env WriterActor
    scheduleHeartbeat env

    fork $ exchange env

--------------------------------------------------------------------------------
data Env =
  Env { _conn     :: ClientConnection
      , _opExec   :: OperationExec
      , _setts    :: Settings
      , _msgNum   :: IORef Integer
      , _msgQueue :: Chan Msg
      , _pkgQueue :: Chan Pkg
      }

--------------------------------------------------------------------------------
newEnv :: Settings -> OperationExec -> ClientConnection -> IO Env
newEnv setts opExec conn =
  Env conn opExec setts <$> newIORef 0
                        <*> newChan
                        <*> newChan

--------------------------------------------------------------------------------
data Msg
  = Recv Pkg
  | Heartbeat Integer
  | HeartbeatTimeout Integer
  | Stop

--------------------------------------------------------------------------------
data ActorType = ReceiverActor | WriterActor

--------------------------------------------------------------------------------
receiver :: Chan Msg -> ClientConnection -> IO ()
receiver msgQueue c = forever $ do
  pkg <- recv c
  writeChan msgQueue (Recv pkg)

--------------------------------------------------------------------------------
writer :: Chan Pkg -> ClientConnection -> IO ()
writer pkgQueue conn = forever $ do
  pkg <- readChan pkgQueue
  send conn pkg

--------------------------------------------------------------------------------
spawn :: Env -> ActorType -> IO ()
spawn Env{..} tpe = do
  let action =
        case tpe of
          ReceiverActor -> receiver _msgQueue _conn
          WriterActor   -> writer _pkgQueue _conn

  _ <- forkFinally action $ \_ ->
         writeChan _msgQueue Stop

  return ()

--------------------------------------------------------------------------------
scheduleHeartbeat :: Env -> IO ()
scheduleHeartbeat Env{..} = do
  num <- readIORef _msgNum
  delayed (heartbeatInterval _setts) $
    writeChan _msgQueue (Heartbeat num)

--------------------------------------------------------------------------------
scheduleHeartbeatTimeout :: Env -> IO ()
scheduleHeartbeatTimeout Env{..} = do
  num <- readIORef _msgNum
  delayed (heartbeatTimeout _setts) $
    writeChan _msgQueue (HeartbeatTimeout num)

--------------------------------------------------------------------------------
incrMsgNum :: Env -> IO ()
incrMsgNum Env{..} = atomicModifyIORef' _msgNum $ \i -> (succ 1, ())

--------------------------------------------------------------------------------
exchange :: Env -> IO ()
exchange env@Env{..} = forever $ do
  msg <- readChan _msgQueue
  case msg of
    Recv pkg -> do
      incrMsgNum env

      for_ (parseOp pkg) $ \(SomeOperation op) -> do
        outcome <- executeOperation _opExec op

        case outcome of
          OpSend opPkg ->
            writeChan _pkgQueue opPkg

          OpNoop ->
            return ()

    Heartbeat num -> do
      cur <- readIORef _msgNum

      if cur /= num
        then scheduleHeartbeat env
        else do
          pkg <- heartbeatRequest
          writeChan _pkgQueue pkg
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
