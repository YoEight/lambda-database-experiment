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
exec :: Settings -> IO ()
exec setts = do
  mvar      <- newEmptyMVar
  conn      <- newServerConnection $ connectionSettings setts
  mainBus   <- newBus "main-bus"
  mainQueue <- newQueuePublisher "main-queue" mainBus
  newOperationExec setts mainBus mainQueue

  subscribe mainBus (onShutdown mvar)
  publish mainQueue SystemInit

  takeMVar mvar

--------------------------------------------------------------------------------
onShutdown :: MVar () -> Shutdown -> IO ()
onShutdown mvar _ = putMVar mvar ()