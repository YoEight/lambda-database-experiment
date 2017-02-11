{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Protocol.Types
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Protocol.Types where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Hashable
import Data.Serialize
import Data.UUID hiding (fromString)
import Data.UUID.V4

--------------------------------------------------------------------------------
newtype Data = Data ByteString deriving Eq

--------------------------------------------------------------------------------
instance IsString Data where
  fromString = Data . fromString

--------------------------------------------------------------------------------
emptyData :: Data
emptyData = Data mempty

--------------------------------------------------------------------------------
dataBytes :: Data -> ByteString
dataBytes (Data bs) = bs

--------------------------------------------------------------------------------
instance Show Data where
  show _ = "Data(*Binary data*)"

--------------------------------------------------------------------------------
instance Serialize Data where
  get = Data <$> get
  put (Data bs) = put bs

--------------------------------------------------------------------------------
-- | Used to store a set a properties. One example is to be used as 'Event'
--   metadata.
newtype Properties = Properties (Map Text Text) deriving Eq

--------------------------------------------------------------------------------
instance Monoid Properties where
  mempty = Properties mempty
  mappend (Properties a) (Properties b) = Properties $ mappend a b

--------------------------------------------------------------------------------
instance Show Properties where
  show (Properties m) = show m

--------------------------------------------------------------------------------
instance Serialize Properties where
  put (Properties m) =
    for_ (mapToList m) $ \(key, value) -> do
      put $ encodeUtf8 key
      put $ encodeUtf8 value

  get =
    let action = do
          (,) <$> fmap decodeUtf8 get
              <*> fmap decodeUtf8 get in
    (Properties . mapFromList) <$> some action

--------------------------------------------------------------------------------
-- | Retrieves a value associated with the given key.
property :: MonadPlus m => Text -> Properties -> m Text
property k (Properties m) =
  case lookup k m of
    Nothing -> mzero
    Just v -> return v

--------------------------------------------------------------------------------
-- | Builds a 'Properties' with a single pair of key-value.
singleton :: Text -> Text -> Properties
singleton k v = setProperty k v mempty

--------------------------------------------------------------------------------
-- | Adds a pair of key-value into given 'Properties'.
setProperty :: Text -> Text -> Properties -> Properties
setProperty key value (Properties m) = Properties $ insertMap key value m

--------------------------------------------------------------------------------
-- | Returns all associated key-value pairs as a list.
properties :: Properties -> [(Text, Text)]
properties (Properties m) = mapToList m

--------------------------------------------------------------------------------
-- | Used to identify an event.
newtype EventId = EventId UUID deriving (Eq, Ord)

--------------------------------------------------------------------------------
instance Hashable EventId where
  hashWithSalt x (EventId i) = hashWithSalt x i

--------------------------------------------------------------------------------
instance Serialize EventId where
  get = do
    bs <- get
    case fromByteString bs of
      Just uuid -> return $ EventId uuid
      Nothing   -> mzero

  put (EventId uuid) = put (toByteString uuid)

--------------------------------------------------------------------------------
eventIdBytes :: EventId -> ByteString
eventIdBytes (EventId uuid) = toStrict $ toByteString uuid

--------------------------------------------------------------------------------
instance Show EventId where
  show (EventId uuid) = show uuid

--------------------------------------------------------------------------------
-- | Generates a fresh 'EventId'.
freshEventId :: MonadIO m => m EventId
freshEventId = fmap EventId $ liftIO nextRandom

--------------------------------------------------------------------------------
eventIdByteString :: EventId -> ByteString
eventIdByteString (EventId uuid) = toStrict $ toByteString uuid

--------------------------------------------------------------------------------
-- | Represents a stream name.
newtype StreamName = StreamName { streamName :: Text } deriving (Eq, Ord)

--------------------------------------------------------------------------------
instance Hashable StreamName where
  hashWithSalt x (StreamName n) = hashWithSalt x n

--------------------------------------------------------------------------------
instance Show StreamName where
  show (StreamName s) = show s

--------------------------------------------------------------------------------
instance Serialize StreamName where
  get = (StreamName . decodeUtf8) <$> get

  put (StreamName n) = put $ encodeUtf8 n

--------------------------------------------------------------------------------
instance IsString StreamName where
  fromString = StreamName . fromString

--------------------------------------------------------------------------------
-- | Used to identity the type of an 'Event'.
newtype EventType = EventType Text deriving Eq

--------------------------------------------------------------------------------
instance Serialize EventType where
  get = (EventType . decodeUtf8) <$> get

  put (EventType tpe) = put $ encodeUtf8 tpe

--------------------------------------------------------------------------------
eventTypeText :: EventType -> Text
eventTypeText (EventType t) = t

--------------------------------------------------------------------------------
instance Show EventType where
  show (EventType t) = show t

--------------------------------------------------------------------------------
instance IsString EventType where
  fromString = EventType . fromString

--------------------------------------------------------------------------------
-- | Encapsulates an event which is about to be saved.
data Event =
  Event { eventType :: EventType
        , eventId :: EventId
        , eventPayload :: Data
        , eventMetadata :: Maybe Properties
        } deriving (Show, Eq)

--------------------------------------------------------------------------------
instance Serialize Event where
  get =
    Event <$> get
          <*> get
          <*> get
          <*> get

  put Event{..} = do
    put eventType
    put eventId
    put eventPayload
    put eventMetadata

--------------------------------------------------------------------------------
-- | Represents an event index in a stream.
newtype EventNumber = EventNumber Int32 deriving (Eq, Ord, Num, Enum, Show)

--------------------------------------------------------------------------------
instance Serialize EventNumber where
  get = fromIntegral <$> getWord32le

  put (EventNumber n) = putWord32le (fromIntegral n)

--------------------------------------------------------------------------------
-- | Represents an event that's saved into the event store.
data SavedEvent =
  SavedEvent { eventNumber :: EventNumber
             , savedEvent :: Event
             } deriving Show

--------------------------------------------------------------------------------
instance Serialize SavedEvent where
  get =
    SavedEvent <$> get
               <*> get

  put SavedEvent{..} = do
    put eventNumber
    put savedEvent

--------------------------------------------------------------------------------
-- | The purpose of 'ExpectedVersion' is to make sure a certain stream state is
--   at an expected point in order to carry out a write.
data ExpectedVersion
  = AnyVersion
    -- Stream is a any given state.
  | NoStream
    -- Stream shouldn't exist.
  | StreamExists
    -- Stream should exist.
  | ExactVersion EventNumber
    -- Stream should be at givent event number.
  deriving Show

--------------------------------------------------------------------------------
-- | Represents batch information needed to read a stream.
data Batch =
  Batch { batchFrom :: EventNumber
        , batchSize :: Int32
        }

--------------------------------------------------------------------------------
-- | Starts a 'Batch' from a given point. The batch size is set to default,
--   which is 500.
startFrom :: EventNumber -> Batch
startFrom from = Batch from 500

--------------------------------------------------------------------------------
data WriteResultFlag
  = WriteSuccess
  | WriteWrongExpectedVersion
  deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
data ReadResultFlag
  = ReadSuccess
  | ReadNoStream
  deriving (Eq, Enum, Show)
