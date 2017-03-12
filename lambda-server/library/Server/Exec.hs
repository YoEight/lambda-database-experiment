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

--------------------------------------------------------------------------------
type ServicePendingInit = Map ServiceName ()

--------------------------------------------------------------------------------
exec :: Settings -> IO ()
exec setts = do
  mvar      <- newEmptyMVar
  conn      <- newServerConnection $ connectionSettings setts
  mainBus   <- newBus "main-bus"
  mainQueue <- newQueuePublisher "main-queue" mainBus
  initMap   <- newIORef initPendingMap
  newOperationExec setts mainBus mainQueue

  subscribe mainBus (onShutdown mvar)
  subscribe mainBus (onInitialized initMap )

  publish mainQueue SystemInit

  takeMVar mvar

--------------------------------------------------------------------------------
initPendingMap :: ServicePendingInit
initPendingMap = foldMap go [minBound..]
  where
    go i = singletonMap i ()

--------------------------------------------------------------------------------
onShutdown :: MVar () -> Shutdown -> IO ()
onShutdown mvar _ = putMVar mvar ()

--------------------------------------------------------------------------------
onInitialized :: IORef ServicePendingInit -> Initialized -> IO ()
onInitialized ref (Initialized svc) = do
  initialized <- atomicModifyIORef' ref $ \m ->
    let m' = deleteMap svc m in
    (m', null m')

  when initialized $
    say "System properly initialized"