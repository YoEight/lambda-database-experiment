{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Test.TransactionLog
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Test.TransactionLog (spec) where

--------------------------------------------------------------------------------
import System.IO hiding (print, readFile, writeFile)

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.Trans.Resource
import Data.Acquire
import Data.Conduit
import Data.Serialize
import Protocol.Types
import System.Directory
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
import Server.Bus
import Server.Messages
import Server.Messaging
import Server.TransactionLog
import Server.Types

--------------------------------------------------------------------------------
freshFilePath :: IO FilePath
freshFilePath = do
  createDirectoryIfMissing True "trash"
  (pid :: Guid) <- freshId
  return $ "trash/" ++ show pid ++ ".test"

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  specify "create new" $ do
    path <- freshFilePath
    bus  <- newBus "create-new"
    chan <- newChan
    var  <- newEmptyMVar
    newBackend path bus bus

    subscribe bus $ \(Initialized tpe) ->
      case tpe of
        TransactionLog ->
          putMVar var True
        _ -> return ()

    subscribe bus $ \(SystemInitFailure svc e) -> do
      print (svc, e)
      putMVar var False

    publish bus SystemInit
    value <- takeMVar var

    value `shouldBe` True

  specify "save" $ do
    path <- freshFilePath
    eid1 <- freshId
    eid2 <- freshId
    chan <- newChan
    bus  <- newBus "save"

    subscribe bus (writeChan chan)

    let e1 = Event "type1" eid1 "payload1" Nothing
        e2 = Event "type2" eid2 "payload2" Nothing

    newBackend path bus bus
    gid <- freshId
    publish bus SystemInit
    publish bus (WritePrepares [e1, e2] AnyVersion gid "save-stream")

    eids <- (fmap preparedEventId . preparedEvents) <$> readChan chan
    eids `shouldBe` [eid1, eid2]

  specify "load" $ do
    path <- freshFilePath
    eid1 <- freshId
    eid2 <- freshId
    bus  <- newBus "load"
    chan <- newChan

    subscribe bus (writeChan chan)

    let e1 = Event "type1" eid1 "payload1" Nothing
        e2 = Event "type2" eid2 "payload2" Nothing

    newBackend path bus bus
    gid <- freshId
    publish bus SystemInit
    publish bus (WritePrepares [e1, e2] AnyVersion gid "load-stream")

    eids <- (fmap preparedEventId . preparedEvents) <$> readChan chan
    ll   <- runResourceT $ sourceToList $ sourceLogs path

    eids `shouldBe` logEventId <$> ll
