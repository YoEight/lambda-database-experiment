--------------------------------------------------------------------------------
-- |
-- Module : Test.Serialize
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Test.Serialize (spec) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Serialize
import Protocol.Types
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
import Test.Common

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  specify "StreamName" $ do
    path <- freshFile "stream"
    writeFile path (encode $ StreamName "stream")
    bs <- readFile path

    decode bs `shouldBe` (Right $ StreamName "stream")

  specify "EventType" $ do
    path <- freshFile "eventtype"
    writeFile path (encode $ EventType "type")
    bs <- readFile path

    decode bs `shouldBe` (Right $ EventType "type")

  specify "EventId" $ do
    eid <- freshId
    path <- freshFile "eventid"
    writeFile path (encode eid)
    bs <- readFile path

    decode bs `shouldBe` Right (eid :: EventId)

  specify "Data" $ do
    path <- freshFile "data"
    writeFile path $ encode $ Data "data"
    bs <- readFile path

    decode bs `shouldBe` (Right $ Data "data")

  specify "Properties" $ do
    path <- freshFile "properties"
    let ps = setProperty "foo" "bar" $
             setProperty "tit" "tot" mempty

    writeFile path $ encode ps
    bs <- readFile path

    decode bs `shouldBe` Right ps

  specify "Event" $ do
    eid <- freshId
    path <- freshFile "event"
    let ps = setProperty "foo" "bar" $
             setProperty "tit" "tot" mempty

        evt = Event { eventType     = "type"
                    , eventId       = eid
                    , eventPayload  = Data "data"
                    , eventMetadata = Just ps
                    }

    writeFile path (encode evt)
    bs <- readFile path

    decode bs `shouldBe` Right evt
