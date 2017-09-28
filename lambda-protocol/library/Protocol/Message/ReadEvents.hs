{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}
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
  , parseOp
  , parseResp
  ) where

--------------------------------------------------------------------------------
import Data.ProtocolBuffers hiding (encode, decode)
import Data.Serialize hiding (Result)
import Lambda.Prelude

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
instance Decode ReadReq

--------------------------------------------------------------------------------
data ReadResp =
  ReadResp { readEvents      :: Repeated 1 (Message EventRecordMsg)
           , readResult      :: Required 2 (Enumeration ReadResultFlag)
           , readNextNumber  :: Required 3 (Value Int32)
           , readEndOfStream :: Required 4 (Value Bool)
           , readRespStream  :: Required 5 (Value Text)
           } deriving  (Generic, Show)

--------------------------------------------------------------------------------
instance Decode ReadResp
instance Encode ReadResp

--------------------------------------------------------------------------------
createPkg :: PkgId -> StreamName -> Batch -> Pkg
createPkg pid (StreamName name) (Batch (EventNumber start) size) =
  Pkg { pkgCmd     = 0x05
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
  Pkg { pkgCmd     = 0x06
      , pkgId      = pid
      , pkgPayload = runPut $ encodeMessage resp
      }
  where
    resp = ReadResp { readResult      = putField flag
                    , readNextNumber  = putField num
                    , readEndOfStream = putField eos
                    , readEvents      = putField $ fmap (toEventRecord name) xs
                    , readRespStream  = putField $ streamName name
                    }

--------------------------------------------------------------------------------
parseOp :: MonadPlus m => Pkg -> m (Operation ReadEventsResp)
parseOp Pkg{..} =
  case pkgCmd of
    0x05 ->
      case runGet decodeMessage pkgPayload of
        Right r -> do
          let streamName = StreamName $ getField $ readStreamId r
              startNum   = EventNumber $ getField $ readStartNum r
              batchSize  = getField $ readBatchSize r

          return $ Operation pkgId
                 $ ReadEvents streamName (Batch startNum batchSize)
        _ -> mzero
    _ -> mzero

--------------------------------------------------------------------------------
parseResp :: MonadPlus m => Pkg -> m (Response ReadEventsResp)
parseResp Pkg{..} =
  case pkgCmd of
    0x06 ->
      case runGet decodeMessage pkgPayload of
        Right r -> do
          let flag    = getField $ readResult r
              nextNum = EventNumber $ getField $ readNextNumber r
              eos     = getField $ readEndOfStream r
              name    = StreamName $ getField $ readRespStream r

          xs <- traverse fromEventRecord $ getField $ readEvents r

          return $ Response pkgId
                 $ ReadEventsResp name xs flag nextNum eos
        _ -> mzero
    _ -> mzero
