--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Client
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Client public API.
--------------------------------------------------------------------------------
module Lambda.Client
  ( Client
  , WriteResult(..)
  , ReadStreamResult(..)
  , newClient
  , newClientWithDefault
  , awaitShutdown
  , writeEvents
  , readEvents
  , module Protocol.Types
  ) where

--------------------------------------------------------------------------------
import Data.List.NonEmpty

--------------------------------------------------------------------------------
import Lambda.Bus
import Lambda.Prelude
import Protocol.Operation
import Protocol.Types hiding (streamName, eventNumber)

--------------------------------------------------------------------------------
import qualified Lambda.Client.Connection as Connection
import qualified Lambda.Client.Messages   as Messages
import           Lambda.Client.Settings
import           Lambda.Client.TcpConnection

--------------------------------------------------------------------------------
-- | Client connection reference.
data Client =
  Client
  { _settings :: Settings
    -- ^ Connection settings.
  , _mainBus :: Bus Settings
    -- ^ Main message bus.
  }

--------------------------------------------------------------------------------
-- | Creates a new connection to a single node. It maintains a full duplex
--   connection to the database. A connection operates quite
--   differently than say a SQL connection. You want to keep the connection open
--   for a much longer of time than when you use a SQL connection.
--
--   Another difference  is all operations are handled in a full async manner
--   (even if you call the synchronous behaviors). Many threads can use a
--   connection at the same time or a single thread can make many asynchronous
--   requests. To get the most performance out of the connection it is generally
--   recommended to use it in this way.
newClient :: Settings -> IO Client
newClient setts = lambdaMain_ setts $ do
  mainBus <- newBus
  builder <- connectionBuilder

  configure mainBus (Connection.app builder)

  let client = Client setts mainBus
  return client

--------------------------------------------------------------------------------
-- | Response you get when sending a write request to the server.
data WriteResult =
  WriteResult
  { eventNumber :: !EventNumber
    -- ^ Next 'EventNumber' of the stream which performed the write.
  , result :: !WriteResultFlag
    -- ^ Write request outcome.
  } deriving Show

--------------------------------------------------------------------------------
-- | Sends a write request.
writeEvents :: Client
            -> StreamName
            -> NonEmpty Event
            -> ExpectedVersion
            -> IO (Async WriteResult)
writeEvents self name events version =
  fmap (fmap convert) $ submitRequest self req
  where
    req = WriteEvents name version events

    convert (WriteEventsResp num flag) =
      WriteResult num flag

--------------------------------------------------------------------------------
-- | Response you get when sending a read request to the server.
data ReadStreamResult =
  ReadStreamResult
  { streamName :: !StreamName
    -- ^ The stream name where the read operation took place.
  , events :: ![SavedEvent]
    -- ^ Current batch of 'SavedEvent''s
  , flag :: !ReadResultFlag
    -- ^ Read request outcome.
  , nextEventNumber :: !EventNumber
    -- ^ Next 'EventNumber' to use if you want to read the next events.
  , endOfStream :: !Bool
    -- ^ If the end of stream has been reached.
  } deriving Show

--------------------------------------------------------------------------------
-- | Sends a read request.
readEvents :: Client -> StreamName -> Batch -> IO (Async ReadStreamResult)
readEvents self name batch =
  fmap (fmap convert) $ submitRequest self req
  where
    req = ReadEvents name batch

    convert (ReadEventsResp n xs fl num eos) =
      ReadStreamResult n xs fl num eos

--------------------------------------------------------------------------------
-- | Waits the 'Client' to carry out all its pending operations.
awaitShutdown :: Client -> IO ()
awaitShutdown Client{..} = busProcessedEverything _mainBus

--------------------------------------------------------------------------------
-- | Creates a 'Client' using 'defaultSettings'.
newClientWithDefault :: IO Client
newClientWithDefault = newClient defaultSettings

--------------------------------------------------------------------------------
-- | Utility function to send asynchronous request.
submitRequest :: Client -> Request a -> IO (Async a)
submitRequest Client{..} req = do
  var <- newEmptyMVar
  let evt = Messages.NewRequest req (putMVar var)
      msg = Message
            { messagePayload = evt
            , messageSender  = _busId _mainBus
            , messageTarget  = Nothing
            }

  _ <- atomically $ publishSTM _mainBus msg
  async (either throwString pure =<< takeMVar var)
