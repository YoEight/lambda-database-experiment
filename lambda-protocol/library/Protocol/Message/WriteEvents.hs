{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
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
module Protocol.Message.WriteEvents
  ( Result(..)
  , createPackage
  , parseOperation
  ) where

--------------------------------------------------------------------------------
import Data.List.NonEmpty hiding (toList)

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
data EventMsg =
  EventMsg { msgId       :: Required 1 (Value ByteString)
           , msgType     :: Required 2 (Value Text)
           , msgData     :: Required 3 (Value ByteString)
           , msgMetadata :: Optional 4 (Value ByteString)
           } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Encode EventMsg
instance Decode EventMsg

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
instance Decode WriteReq

--------------------------------------------------------------------------------
data WriteResp =
  WriteResp { writeResult     :: Required 1 (Enumeration Result)
            , writeNextNumber :: Required 2 (Value Int32)
            } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance Decode WriteResp

--------------------------------------------------------------------------------
verInt32 :: ExpectedVersion -> Int32
verInt32 AnyVersion         = -2
verInt32 NoStream           = -1
verInt32 StreamExists       = 0
verInt32 (ExactVersion num) = let EventNumber n = num in n

--------------------------------------------------------------------------------
int32ver :: Int32 -> ExpectedVersion
int32ver (-2) = AnyVersion
int32ver (-1) = NoStream
int32ver 0    = StreamExists
int32ver n    = ExactVersion $ EventNumber n

--------------------------------------------------------------------------------
createPackage :: StreamName -> ExpectedVersion -> NonEmpty Event -> IO Pkg
createPackage name ver xs = do
  pid <- freshPkgId
  return Pkg { pkgCmd      = 0x02
              , pkgId      = pid
              , pkgPayload = runPut $ encodeMessage req
              }
  where
    req = WriteReq { writeStreamId = putField $ streamName name
                   , writeExpectedVersion = putField $ verInt32 ver
                   , writeMsgs = putField $ toList $ fmap toMsg xs
                   }

    toMsg e =
      EventMsg { msgId       = putField $ eventIdByteString $ eventId e
               , msgType     = putField $ eventTypeText $ eventType e
               , msgData     = putField $ dataBytes $ eventPayload e
               , msgMetadata = putField $ fmap encode $ eventMetadata e
               }

--------------------------------------------------------------------------------
parseOperation :: MonadPlus m => Pkg -> m Operation
parseOperation Pkg{..} =
  case pkgCmd of
    0x02 ->
      case runGet decodeMessage pkgPayload of
        Right r -> do
          let name = StreamName $ getField $ writeStreamId r
              ver  = int32ver $ getField $ writeExpectedVersion r
              xs   = getField $ writeMsgs r

              toEvt msg = do
                eid <- case fromByteString $ fromStrict $ getField $ msgId msg of
                         Just uuid -> return $ EventId uuid
                         _         -> mzero

                let dat = getField $ msgMetadata msg
                return Event { eventId       = eid
                             , eventType     = EventType $ getField $ msgType msg
                             , eventPayload  = Data $ getField $ msgData msg
                             , eventMetadata = eitherMaybe . decode =<< dat
                             }

          evts     <- traverse toEvt xs
          safeEvts <- maybe mzero return $ nonEmpty evts

          return $ WriteEvents name ver safeEvts
        _ -> mzero
    _ -> mzero

--------------------------------------------------------------------------------
eitherMaybe :: Either e a -> Maybe a
eitherMaybe (Right a) = Just a
eitherMaybe _         = Nothing
