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
--------------------------------------------------------------------------------
module Lambda.Client
  ( Client
  , newClient
  , newClientWithDefault
  , awaitShutdown
  , writeEvents
  ) where

--------------------------------------------------------------------------------
import Data.List.NonEmpty

--------------------------------------------------------------------------------
import Lambda.Bus
import Lambda.Prelude
import Protocol.Operation
import Protocol.Types

--------------------------------------------------------------------------------
import qualified Lambda.Client.Connection as Connection
import qualified Lambda.Client.Messages   as Messages
import           Lambda.Client.Settings
import           Lambda.Client.TcpConnection

--------------------------------------------------------------------------------
data Client =
  Client
  { _settings :: Settings
  , _mainBus  :: Bus Settings
  }

--------------------------------------------------------------------------------
newClient :: Settings -> IO Client
newClient setts = lambdaMain_ setts $ do
  mainBus <- newBus
  builder <- connectionBuilder

  configure mainBus (Connection.app builder)

  let client = Client setts mainBus
  return client

--------------------------------------------------------------------------------
data WriteResult =
  WriteResult
  { eventNumber :: !EventNumber
  , result      :: !WriteResultFlag
  }

--------------------------------------------------------------------------------
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
awaitShutdown :: Client -> IO ()
awaitShutdown Client{..} = busProcessedEverything _mainBus

--------------------------------------------------------------------------------
newClientWithDefault :: IO Client
newClientWithDefault = newClient defaultSettings

--------------------------------------------------------------------------------
submitRequest :: Client -> Request a -> IO (Async a)
submitRequest Client{..} req = do
  var <- newEmptyMVar
  let evt = Messages.NewRequest req (putMVar var)
      msg = Message
            { messagePayload = evt
            , messageSender  = _busId _mainBus
            , messageTarget  = Nothing
            }

  atomically $ publishSTM _mainBus msg
  async (either throwString pure =<< takeMVar var)
