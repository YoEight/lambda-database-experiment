{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.ProtocolBuffers hiding (encode, decode)
import Data.Serialize
import Data.UUID

--------------------------------------------------------------------------------
import Protocol.Types

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
instance Encode EventRecordMsg
instance Decode EventRecordMsg

--------------------------------------------------------------------------------
toEventRecord :: StreamName -> SavedEvent -> EventRecordMsg
toEventRecord (StreamName name) (SavedEvent (EventNumber num) Event{..}) =
  EventRecordMsg { eventMsgStreamId = putField name
                 , eventMsgNumber   = putField num
                 , eventMsgId       = putField $ eventIdBytes eventId
                 , eventMsgType     = putField $ eventTypeText eventType
                 , eventMsgData     = putField $ dataBytes eventPayload
                 , eventMsgMetadata = putField $ fmap encode eventMetadata
                 }

--------------------------------------------------------------------------------
fromEventRecord :: MonadPlus m => EventRecordMsg -> m SavedEvent
fromEventRecord em = do
  eid <-
    case fromByteString $ fromStrict $ getField $ eventMsgId em of
      Just uuid -> return $ EventId uuid
      _         -> mzero

  let dat     = Data $ getField $ eventMsgData em
      metadat = eitherMaybe . decode =<< getField (eventMsgMetadata em)
      typ     = EventType $ getField $ eventMsgType em

  return SavedEvent { eventNumber = EventNumber $ getField $ eventMsgNumber em
                    , savedEvent  =
                        Event { eventType     = typ
                              , eventId       = eid
                              , eventPayload  = dat
                              , eventMetadata = metadat
                              }
                    }

--------------------------------------------------------------------------------
eitherMaybe :: Either e a -> Maybe a
eitherMaybe (Right a) = Just a
eitherMaybe _         = Nothing
