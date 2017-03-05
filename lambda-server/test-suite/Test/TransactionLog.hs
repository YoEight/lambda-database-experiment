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
import Data.Conduit
import Data.Serialize
import Protocol.Types
import System.Directory
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
import Server.Bus
import Server.Messaging
import Server.TransactionLog

--------------------------------------------------------------------------------
freshFilePath :: IO FilePath
freshFilePath = do
  createDirectoryIfMissing True "trash"
  (pid :: Guid) <- freshId
  return $ "trash/" ++ show pid ++ ".test"

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  specify "transaction-id" $ do
    path <- freshFilePath
    tid <- freshId
    writeFile path (encode (tid :: TransactionId))
    bs <- readFile path

    decode bs `shouldBe` Right tid

  specify "header" $ do
    path <- freshFilePath

    let action = bracketP (openFile path ReadWriteMode) hClose $ \h ->
          initializeHeader h 32

    runResourceT $ runConduit action

    let action2 = bracketP (openFile path ReadMode) hClose $ \h ->
          getLastSeqNum h

    header <- runResourceT $ runConduit action2

    header `shouldBe` 32

  specify "create new" $ do
    path <- freshFilePath
    bus  <- newBus "create-new"
    _    <- newBackend path bus

    return ()

  specify "save" $ do
    path <- freshFilePath
    eid1 <- freshId
    eid2 <- freshId
    chan <- newChan
    bus  <- newBus "save"

    subscribe bus (writeChan chan)

    let e1 = Event "type1" eid1 "payload1" Nothing
        e2 = Event "type2" eid2 "payload2" Nothing

    b <- newBackend path bus

    save b "save-1" [e1, e2]

    Prepared t1 eeid1 seq1 <- readChan chan
    Prepared t2 eeid2 seq2 <- readChan chan
    Committed _ tt         <- readChan chan

    eeid1 `shouldBe` eid1
    eeid2 `shouldBe` eid2
    t1    `shouldBe` t2
    tt    `shouldBe` t1

    seq1 `shouldSatisfy` (< seq2)

    let action = bracketP (openFile path ReadMode) hClose $ \h ->
          getLastSeqNum h

    header <- runResourceT $ runConduit action

    header `shouldBe` 3

  specify "load" $ do
    path <- freshFilePath
    eid1 <- freshId
    eid2 <- freshId
    bus  <- newBus "load"
    chan <- newChan

    subscribe bus (writeChan chan)

    let e1 = Event "type1" eid1 "payload1" Nothing
        e2 = Event "type2" eid2 "payload2" Nothing

    b <- newBackend path bus

    save b "load-1" [e1, e2]

    let looping = do
          msg <- readChan chan
          case msg of
            Prepared _ _ i -> fmap ((i, Prepare):)looping
            Committed i _  -> return [(i, Commit)]

    expSeqs <- looping
    ll <- runResourceT $ sourceToList $ logs b

    let seqs = fmap (\l -> (logSeqNum l, logType l)) ll
        sum  = foldl' (\x (i, _) -> i + x) 1 seqs

    length ll `shouldBe` 3
    seqs `shouldBe` expSeqs
    sum `shouldBe` 4
