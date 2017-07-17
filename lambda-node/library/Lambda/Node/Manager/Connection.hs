--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Node.Manager.Connection
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Manager.Connection (new) where

--------------------------------------------------------------------------------
import Network.Simple.TCP

--------------------------------------------------------------------------------
import Data.Serialize
import Protocol.Package

--------------------------------------------------------------------------------
import           Lambda.Node.Bus
import           Lambda.Node.Logger
import qualified Lambda.Node.Manager.Timer as Timer
import           Lambda.Node.Monitoring
import           Lambda.Node.Prelude
import           Lambda.Node.Settings
import           Lambda.Node.Types

--------------------------------------------------------------------------------
data ServerSocket =
  ServerSocket
  { _serverSocket :: !Socket
  , _serverAddr   :: !SockAddr
  }

--------------------------------------------------------------------------------
data ClientSocket =
  ClientSocket
  { _clientSocket :: !Socket
  , _clientAddr   :: !SockAddr
  , _clientId     :: !UUID
  , _clientBus    :: !Bus
  , _clientRecv   :: !(Async ())
  , _clientSend   :: !(Async ())
  , _clientQueue  :: !(TBMQueue Pkg)
  , _clientPkgNum :: !(IORef Integer)
  }

--------------------------------------------------------------------------------
type Connections = HashMap UUID ClientSocket

--------------------------------------------------------------------------------
data Internal =
  Internal
  { _runtime     :: Runtime
  , _mainHub     :: Hub
  , _connections :: IORef Connections
  }

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------
newtype PackageArrived = PackageArrived Pkg

--------------------------------------------------------------------------------
data Routed where
  Routed :: Typeable msg => UUID -> msg -> Routed

--------------------------------------------------------------------------------
data Tick = Tick

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
new :: PubSub h => h -> Runtime -> Settings -> IO ()
new hub runtime setts = do
  self <- Internal runtime (asHub hub) <$> newIORef mempty

  subscribe hub (onRouted self)

  servingFork self (connectionSettings setts)

--------------------------------------------------------------------------------
-- | TODO - Makes sure to cleanup everything in case of exception.
whenClientConnect :: Internal -> Socket -> SockAddr -> IO ()
whenClientConnect Internal{..} sock addr = do
  client <- mfix $ \self ->
    ClientSocket sock addr <$> freshUUID
                           <*> newBus _runtime [i|bus-client-#{_clientId self}|]
                           <*> async (processingIncomingPackage self)
                           <*> async (processingOutgoingPackage self)
                           <*> newTBMQueueIO 500
                           <*> newIORef 0

  atomicModifyIORef' _connections $ \m ->
    (insertMap (_clientId client) client m, ())

  subscribe (_clientBus client) (onPackageArrived client)
  subscribe (_clientBus client) (onTick client)

  let ticking = Routed (_clientId client) Tick
      timer   = Timer.Register ticking 0.2 False

  publishWith _mainHub timer
  busProcessedEverything $ _clientBus client

--------------------------------------------------------------------------------
servingFork :: Internal -> ConnectionSettings -> IO ()
servingFork self ConnectionSettings{..} = void $ fork $
  serve (Host hostname) (show portNumber) $ uncurry (whenClientConnect self)

--------------------------------------------------------------------------------
processingIncomingPackage :: ClientSocket -> IO ()
processingIncomingPackage self@ClientSocket{..} = forever $ do
  prefixBytes <- recvExact self 4
  case decode prefixBytes of
    Left _    -> throwString "Wrong package framing."
    Right len -> do
      payload <- recvExact self len
      case decode payload of
        Left e    -> throwString [i|Package parsing error #{e}.|]
        Right pkg -> publishWith _clientBus (PackageArrived pkg)

--------------------------------------------------------------------------------
recvExact :: ClientSocket -> Int -> IO ByteString
recvExact ClientSocket{..} start = loop mempty start
  where
    loop acc 0 = return acc
    loop acc want = do
      recv _clientSocket want >>= \case
        Nothing -> throwString "Remote end close the connection"
        Just bs
          | length bs == want -> return (acc <> bs)
          | otherwise -> loop (acc <> bs) (want - length bs)

--------------------------------------------------------------------------------
processingOutgoingPackage :: ClientSocket -> IO ()
processingOutgoingPackage ClientSocket{..} = forever $ do
  msgs <- atomically nextBatchSTM
  sendMany _clientSocket msgs
  where
    nextBatchSTM = do
      let loop = do
            tryReadTBMQueue _clientQueue >>= \case
              Nothing   -> fail "Queue is closed"
              Just mMsg ->
                case mMsg of
                  Nothing  -> return []
                  Just pkg -> fmap (encode pkg:) loop

      loop >>= \case
        [] -> retrySTM
        xs -> return xs

--------------------------------------------------------------------------------
enqueuePkg :: ClientSocket -> Pkg -> Server ()
enqueuePkg ClientSocket{..} pkg = atomically $ writeTBMQueue _clientQueue pkg

--------------------------------------------------------------------------------
incrPkgNum :: ClientSocket -> Server ()
incrPkgNum ClientSocket{..} = atomicModifyIORef' _clientPkgNum $
  \n -> (succ n, ())

--------------------------------------------------------------------------------
-- Event Handlers
--------------------------------------------------------------------------------
onRouted :: Internal -> Routed -> Server ()
onRouted Internal{..} (Routed clientId msg) =
  traverse_ dispatch . lookup clientId =<< readIORef _connections
  where
    dispatch ClientSocket{..} = publishWith _clientBus msg

--------------------------------------------------------------------------------
onTick :: ClientSocket -> Tick -> Server ()
onTick ClientSocket{..} _ = logDebug [i|Client #{_clientId}: Ticking...|]

--------------------------------------------------------------------------------
onPackageArrived :: ClientSocket -> PackageArrived -> Server ()
onPackageArrived self@ClientSocket{..} (PackageArrived Pkg{..}) = do
  incrPkgNum self

  case pkgCmd of
    0x01 -> enqueuePkg self (heartbeatResponse pkgId)
    0x02 -> return ()
    _    -> logDebug "Received a request not handled yet."