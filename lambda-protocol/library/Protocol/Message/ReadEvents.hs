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
module Protocol.Message.ReadEvents
  ( createPkg
  , createRespPkg
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.ProtocolBuffers hiding (encode, decode)
import Data.Serialize hiding (Result)
import Data.UUID

--------------------------------------------------------------------------------
import Protocol.Operation
import Protocol.Package
import Protocol.Types

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
data ReadResp =
  ReadResp { readEvents      :: Repeated 1 (Message EventRecordMsg)
           , readResult      :: Required 2 (Enumeration ReadResultFlag)
           , readNextNumber  :: Required 3 (Value Int32)
           , readEndOfStream :: Required 4 (Value Bool)
           } deriving  (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ReadResp
instance Encode ReadResp

--------------------------------------------------------------------------------
createPkg :: StreamName -> Batch -> IO Pkg
createPkg (StreamName name) (Batch (EventNumber start) size) = do
  pid <- freshPkgId
  return Pkg { pkgCmd     = 0x04
             , pkgId      = pid
             , pkgPayload = runPut $ encodeMessage req
             }
  where
    req = ReadReq { readStreamId  = putField name
                  , readStartNum  = putField start
                  , readBatchSize = putField size
                  }

--------------------------------------------------------------------------------
createRespPkg :: PkgId
              -> StreamName
              -> [SavedEvent]
              -> ReadResultFlag
              -> EventNumber
              -> Bool
              -> Pkg
createRespPkg pid name xs flag (EventNumber num) eos =
  Pkg { pkgCmd     = 0x05
      , pkgId      = pid
      , pkgPayload = runPut $ encodeMessage resp
      }
  where
    resp = ReadResp { readResult      = putField flag
                    , readNextNumber  = putField num
                    , readEndOfStream = putField eos
                    , readEvents      = putField $ fmap (toEventRecord name) xs
                    }
