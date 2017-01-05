{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Protocol.Message.ReadEvents
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Protocol.Message.ReadEvents where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.ProtocolBuffers

--------------------------------------------------------------------------------
import Protocol.Message.EventRecord

--------------------------------------------------------------------------------
data ReadReq =
  ReadReq { readStreamId  :: Required 1 (Value Text)
          , readStartNum  :: Required 2 (Value Int32)
          , readBatchSize :: Required 3 (Value Int32)
          } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode ReadReq

--------------------------------------------------------------------------------
data Result = Success
            | NoStream
            deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
data ReadResp =
  ReadResp { readEvents      :: Repeated 1 (Message EventRecordMsg)
           , readResult      :: Required 2 (Enumeration Result)
           , readNextNumber  :: Required 3 (Value Int32)
           , readEndOfStream :: Required 4 (Value Bool)
           } deriving  (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ReadResp
