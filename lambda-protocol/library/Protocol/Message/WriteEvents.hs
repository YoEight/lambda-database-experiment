
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Protocol.Message.WriteEvents
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Protocol.Message.WriteEvents where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
data EventMsg =
  EventMsg { msgId       :: Required 1 (Value ByteString)
           , msgType     :: Required 2 (Value Text)
           , msgData     :: Required 3 (Value ByteString)
           , msgMetadata :: Optional 4 (Value ByteString)
           } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode EventMsg

--------------------------------------------------------------------------------
data Result = Success
            | WrongExpectedVersion
            deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
data WriteReq =
  WriteReq { writeStreamId        :: Required 1 (Value Text)
           , writeExpectedVersion :: Required 2 (Value Int32)
           , writeMsgs            :: Repeated 3 (Message EventMsg)
           } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode WriteReq

--------------------------------------------------------------------------------
data WriteResp =
  WriteResp { writeResult     :: Required 1 (Enumeration Result)
            , writeNextNumber :: Required 2 (Value Int32)
            } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode WriteResp
