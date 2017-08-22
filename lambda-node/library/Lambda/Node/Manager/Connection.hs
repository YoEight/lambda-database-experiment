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
import Lambda.Bus
import Lambda.Logger
import Lambda.Prelude
import Protocol.Package

--------------------------------------------------------------------------------
import           Lambda.Node.Monitoring
import           Lambda.Node.Settings
import           Lambda.Node.Stopwatch

--------------------------------------------------------------------------------
data CheckState
  = CheckInterval
  | CheckTimeout

--------------------------------------------------------------------------------
data HealthTracking =
  HealthTracking
  { _healthLastPkgNum :: !Integer
  , _healthLastCheck  :: !NominalDiffTime
  , _healthCheckState :: !CheckState
  }

--------------------------------------------------------------------------------
initHealthTracking :: HealthTracking
initHealthTracking = HealthTracking 0 0 CheckInterval

--------------------------------------------------------------------------------
manageHeartbeat :: ClientSocket -> Lambda Settings ()
manageHeartbeat self@ClientSocket{..} = do
  setts   <- getSettings
  pkgNum  <- readIORef _clientPkgNum
  track   <- readIORef _clientHealth
  elapsed <- stopwatchElapsed _clientStopwatch

  let duration =
        case _healthCheckState track of
          CheckInterval -> heartbeatInterval setts
          CheckTimeout  -> heartbeatTimeout setts

  if pkgNum > _healthLastPkgNum track
    then writeIORef _clientHealth (HealthTracking pkgNum elapsed CheckInterval)
  else
    when (elapsed - _healthLastCheck track >= duration) $
      case _healthCheckState track of
        CheckInterval -> do
          pkg <- liftIO heartbeatRequest
          enqueuePkg self pkg
          let newTrack = HealthTracking pkgNum elapsed CheckTimeout
          atomicWriteIORef _clientHealth newTrack
        CheckTimeout -> do
          logWarn [i|Connection #{_clientId} closed: Heartbeat timeout.|]
          closeConnection self "HEARTBEAT TIMEOUT"

--------------------------------------------------------------------------------
data ClientSocket =
  ClientSocket
  { _clientSocket    :: !Socket
  , _clientAddr      :: !SockAddr
  , _clientId        :: !UUID
  , _clientBus       :: !Bus
  , _clientStopwatch :: !Stopwatch
  , _clientRecv      :: !(Async ())
  , _clientSend      :: !(Async ())
  , _clientQueue     :: !(TBMQueue Pkg)
  , _clientPkgNum    :: !(IORef Integer)
  , _clientHealth    :: !(IORef HealthTracking)
  , _clientClosing   :: !(IORef Bool)
  }

--------------------------------------------------------------------------------
type Connections = HashMap UUID ClientSocket

--------------------------------------------------------------------------------
data Internal =
  Internal
  {  _connections :: IORef Connections
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
data ConnectionClosed = ConnectionClosed String

--------------------------------------------------------------------------------
data NewConnection = NewConnection

--------------------------------------------------------------------------------
data StartListening = StartListening

--------------------------------------------------------------------------------
app :: Configure Settings ()
app = initialize $ do
  self <- Internal <$> newIORef mempty

  subscribe (onStartListening self)

--------------------------------------------------------------------------------
onStartListening :: Internal -> StartListening -> React Settings ()
onStartListening self _ =
  servingFork self . connectionSettings =<< reactSettings

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
new :: PubSub h => h -> Runtime -> Settings -> IO ()
new hub runtime setts = do
  self <- Internal runtime (asHub hub) <$> newIORef mempty

  subscribe hub (onRouted self)

  servingFork self (connectionSettings setts)

--------------------------------------------------------------------------------
whenClientConnect :: Internal -> Socket -> SockAddr -> Lambda Settings ()
whenClientConnect Internal{..} sock addr = do
  client <- mfix $ \self -> do
    ClientSocket sock addr <$> freshUUID
                           <*> newBus _runtime [i|bus-client-#{_clientId self}|]
                           <*> newStopwatch
                           <*> async (processingIncomingPackage self)
                           <*> async (processingOutgoingPackage self)
                           <*> newTBMQueueIO 500
                           <*> newIORef 0
                           <*> newIORef initHealthTracking
                           <*> newIORef False

  atomicModifyIORef' _connections $ \m ->
    (insertMap (_clientId client) client m, ())

  subscribe (_clientBus client) (onNewConnection client)
  subscribe (_clientBus client) (onPackageArrived client)
  subscribe (_clientBus client) (onTick client)
  subscribe (_clientBus client) (onConnectionClosed client)

  let ticking = Routed (_clientId client) Tick
      timer   = Timer.Register ticking 0.2 False

  publishWith (_clientBus client) NewConnection
  publishWith _mainHub timer

  busProcessedEverything $ _clientBus client

--------------------------------------------------------------------------------
servingFork :: Internal -> ConnectionSettings -> IO ()
servingFork self ConnectionSettings{..} = void $ fork $
  serve (Host hostname) (show portNumber) $ uncurry (whenClientConnect self)

--------------------------------------------------------------------------------
processingIncomingPackage :: ClientSocket -> IO ()
processingIncomingPackage self@ClientSocket{..} = handleAny (onError self) $ forever $ do
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
processingOutgoingPackage self@ClientSocket{..} = handleAny (onError self) $ forever $ do
  msgs <- atomically nextBatchSTM
  sendMany _clientSocket msgs
  where
    nextBatchSTM = do
      let loop = do
            tryReadTBMQueue _clientQueue >>= \case
              Nothing   -> fail [i|Connection #{_clientId} queue closed.|]
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
closeConnection :: ClientSocket -> String -> Server ()
closeConnection ClientSocket{..} reason = do
  done <- atomicModifyIORef' _clientClosing $ \b -> (True, b)
  unless done $ do
    busStop _clientBus
    atomically $ closeTBMQueue _clientQueue
    logInfo [i|Connection #{_clientId} closed, reason: #{reason}.|]

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
onTick self _ = manageHeartbeat self

--------------------------------------------------------------------------------
onPackageArrived :: ClientSocket -> PackageArrived -> Server ()
onPackageArrived self@ClientSocket{..} (PackageArrived Pkg{..}) = do
  incrPkgNum self
  logDebug [i|Package #{pkgId} arrived.|]

  case pkgCmd of
    0x01 -> enqueuePkg self (heartbeatResponse pkgId)
    0x02 -> return ()
    _    -> logDebug "Received a request not handled yet."

--------------------------------------------------------------------------------
onError :: ClientSocket -> SomeException -> IO ()
onError ClientSocket{..} e =
  publishWith _clientBus (ConnectionClosed $ show e)

--------------------------------------------------------------------------------
onNewConnection :: ClientSocket -> NewConnection -> Server ()
onNewConnection ClientSocket{..} _ =
  logInfo [i|New connection #{_clientId} on #{_clientAddr}|]

--------------------------------------------------------------------------------
onConnectionClosed :: ClientSocket -> ConnectionClosed -> Server ()
onConnectionClosed self (ConnectionClosed reason) =
  closeConnection self reason
