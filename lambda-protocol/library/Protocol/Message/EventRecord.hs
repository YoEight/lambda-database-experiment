{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Protocol.Message.EventRecord
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Protocol.Message.EventRecord where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
data EventRecordMsg =
  EventRecordMsg { eventMsgStreamId :: Required 1  (Value Text)
                 , eventMsgNumber   :: Required 2  (Value Int32)
                 , eventMsgId       :: Required 3  (Value ByteString)
                 , eventMsgType     :: Required 4  (Value Text)
                 , eventMsgData     :: Required 7  (Value ByteString)
                 , eventMsgMetadata :: Optional 8  (Value ByteString)
                 } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode EventRecordMsg
