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
import Lambda.Node.Settings
import Lambda.Node.Stopwatch

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
manageHeartbeat :: ClientSocket -> React Settings ()
manageHeartbeat self@ClientSocket{..} = do
  setts   <- reactSettings
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
          clientId <- reactSelfId
          logWarn [i|Connection #{clientId} closed: Heartbeat timeout.|]
          closeConnection self "HEARTBEAT TIMEOUT"

--------------------------------------------------------------------------------
data ClientSocket =
  ClientSocket
  { _clientSocket    :: !Socket
  , _clientAddr      :: !SockAddr
  , _clientStopwatch :: !Stopwatch
  , _clientQueue     :: !(TBMQueue Pkg)
  , _clientPkgNum    :: !(IORef Integer)
  , _clientHealth    :: !(IORef HealthTracking)
  , _clientClosing   :: !(IORef Bool)
  }

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------
newtype PackageArrived = PackageArrived Pkg

--------------------------------------------------------------------------------
data Tick = Tick

--------------------------------------------------------------------------------
data ConnectionClosed = ConnectionClosed String

--------------------------------------------------------------------------------
app :: Bus Settings -> Configure Settings ()
app mainBus = appStart (startListening mainBus)

--------------------------------------------------------------------------------
startListening :: Bus Settings -> React Settings ()
startListening mainBus =
  servingFork mainBus . connectionSettings =<< reactSettings

--------------------------------------------------------------------------------
new :: Bus Settings -> Lambda Settings ()
new mainBus = do
  configure mainBus (app mainBus)

--------------------------------------------------------------------------------
clientApp :: ClientSocket -> Configure Settings ()
clientApp self = do
  subscribe (onPackageArrived self)
  subscribe (onConnectionClosed self)
  subscribe (onTick self)

  timer Tick 0.2 Undefinitely

  appStart $
    do clientId <- reactSelfId
       _ <- async (processingIncomingPackage self)
       _ <- async (processingOutgoingPackage self)
       logInfo [i|New connection #{clientId} on #{_clientAddr self}|]

--------------------------------------------------------------------------------
newClientSocket :: Socket -> SockAddr -> Configure s ClientSocket
newClientSocket sock addr =
  ClientSocket sock addr
    <$> newStopwatch
    <*> (liftIO $ newTBMQueueIO 500)
    <*> newIORef 0
    <*> newIORef initHealthTracking
    <*> newIORef False

--------------------------------------------------------------------------------
servingFork :: Bus Settings -> ConnectionSettings -> React Settings ()
servingFork mainBus ConnectionSettings{..} = void $ fork $ liftBaseWith $ \run ->
  serve (Host hostname) (show portNumber) $ \(sock, addr) -> run $ reactLambda $
    do child <- busNewChild mainBus
       configure child (clientConf sock addr)
       busProcessedEverything child
  where
    clientConf sock addr =
      newClientSocket sock addr >>= clientApp

--------------------------------------------------------------------------------
processingIncomingPackage :: ClientSocket -> React Settings ()
processingIncomingPackage self@ClientSocket{..} = handleAny onError $ forever $ do
  prefixBytes <- liftIO $ recvExact self 4
  case decode prefixBytes of
    Left _    -> throwString "Wrong package framing."
    Right len -> do
      payload <- liftIO $ recvExact self len
      case decode payload of
        Left e    -> throwString [i|Package parsing error #{e}.|]
        Right pkg -> publish (PackageArrived pkg)

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
processingOutgoingPackage :: ClientSocket -> React Settings ()
processingOutgoingPackage ClientSocket{..} = handleAny onError $ forever $ do
  clientId <- reactSelfId
  msgs <- atomically $ nextBatchSTM clientId
  liftIO $ sendMany _clientSocket msgs
  where
    nextBatchSTM clientId =
      let loop = do
            tryReadTBMQueue _clientQueue >>= \case
              Nothing   -> fail [i|Connection #{clientId} queue closed.|]
              Just mMsg ->
                case mMsg of
                  Nothing  -> return []
                  Just pkg -> fmap (encode pkg:) loop

       in loop >>= \case
            [] -> retrySTM
            xs -> return xs

--------------------------------------------------------------------------------
enqueuePkg :: ClientSocket -> Pkg -> React Settings ()
enqueuePkg ClientSocket{..} pkg = atomically $ writeTBMQueue _clientQueue pkg

--------------------------------------------------------------------------------
incrPkgNum :: ClientSocket -> React Settings ()
incrPkgNum ClientSocket{..} = atomicModifyIORef' _clientPkgNum $
  \n -> (succ n, ())

--------------------------------------------------------------------------------
closeConnection :: ClientSocket -> String -> React Settings ()
closeConnection ClientSocket{..} reason = do
  done     <- atomicModifyIORef' _clientClosing $ \b -> (True, b)
  clientId <- reactSelfId
  unless done $ do
    atomically $ closeTBMQueue _clientQueue
    logInfo [i|Connection #{clientId} closed, reason: #{reason}.|]
    stop

--------------------------------------------------------------------------------
-- Event Handlers
--------------------------------------------------------------------------------
onTick :: ClientSocket -> Tick -> React Settings ()
onTick self _ = manageHeartbeat self

--------------------------------------------------------------------------------
onPackageArrived :: ClientSocket -> PackageArrived -> React Settings ()
onPackageArrived self@ClientSocket{..} (PackageArrived Pkg{..}) = do
  incrPkgNum self
  logDebug [i|Package #{pkgId} arrived.|]

  case pkgCmd of
    0x01 -> enqueuePkg self (heartbeatResponse pkgId)
    0x02 -> return ()
    _    -> logDebug "Received a request not handled yet."

--------------------------------------------------------------------------------
onError :: SomeException -> React s ()
onError e = publish (ConnectionClosed $ show e)

--------------------------------------------------------------------------------
onConnectionClosed :: ClientSocket -> ConnectionClosed -> React Settings ()
onConnectionClosed self (ConnectionClosed reason) =
  closeConnection self reason
