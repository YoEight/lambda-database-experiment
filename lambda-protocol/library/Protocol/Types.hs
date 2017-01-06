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
import Data.Serialize
import Data.UUID hiding (fromString)
import Data.UUID.V4

--------------------------------------------------------------------------------
newtype Data = Data ByteString

--------------------------------------------------------------------------------
dataBytes :: Data -> ByteString
dataBytes (Data bs) = bs

--------------------------------------------------------------------------------
instance Show Data where
  show _ = "Data(*Binary data*)"

--------------------------------------------------------------------------------
-- | Used to store a set a properties. One example is to be used as 'Event'
--   metadata.
newtype Properties = Properties (Map Text Text)

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
      putWord32le $ fromIntegral $ olength key
      put $ encodeUtf8 key
      putWord32le $ fromIntegral $ olength value
      put $ encodeUtf8 value

  get =
    let action = do
          keySize <- fromIntegral <$> getWord32le
          key     <- decodeUtf8 <$> getBytes keySize
          valSize <- fromIntegral <$> getWord32le
          value   <- decodeUtf8 <$> getBytes valSize
          return (key, value) in
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
instance Show StreamName where
  show (StreamName s) = show s

--------------------------------------------------------------------------------
instance IsString StreamName where
  fromString = StreamName . fromString

--------------------------------------------------------------------------------
-- | Used to identity the type of an 'Event'.
newtype EventType = EventType Text deriving Eq

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
        } deriving Show

--------------------------------------------------------------------------------
-- | Represents an event index in a stream.
newtype EventNumber = EventNumber Int32 deriving (Eq, Ord, Num, Enum, Show)

--------------------------------------------------------------------------------
-- | Represents an event that's saved into the event store.
data SavedEvent =
  SavedEvent { eventNumber :: EventNumber
             , savedEvent :: Event
             } deriving Show

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
