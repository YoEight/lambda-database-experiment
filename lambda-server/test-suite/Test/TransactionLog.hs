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
import ClassyPrelude
import Data.UUID.V4
import Protocol.Types
import System.Directory
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
import Server.Messaging
import Server.TransactionLog

--------------------------------------------------------------------------------
freshFilePath :: IO FilePath
freshFilePath = do
  createDirectoryIfMissing True "trash"
  pid <- nextRandom
  return $ "trash/" ++ show pid ++ ".test"

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  specify "create new" $ do
    path   <- freshFilePath
    (_, p) <- newExchange
    _      <- newBackend path p

    return ()

  specify "save" $ do
    path <- freshFilePath
    eid1 <- freshEventId
    eid2 <- freshEventId

    (w, p) <- newExchange

    let e1 = Event "type1" eid1 "payload1" Nothing
        e2 = Event "type2" eid2 "payload2" Nothing

    b <- newBackend path p

    save b "save-1" [e1, e2]

    Prepared t1 eeid1 seq1 <- awaitMsg w
    Prepared t2 eeid2 seq2 <- awaitMsg w
    Committed tt           <- awaitMsg w

    eeid1 `shouldBe` eid1
    eeid2 `shouldBe` eid2
    t1    `shouldBe` t2
    tt    `shouldBe` t1

    seq1 `shouldSatisfy` (< seq2)
