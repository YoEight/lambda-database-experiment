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
import Lambda.Prelude.Stopwatch
import Network.Connection
import Protocol.Package
import Protocol.Message
import Protocol.Operation

--------------------------------------------------------------------------------
import qualified Lambda.Node.Manager.Operation as Operation
import           Lambda.Node.Settings

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
  { _clientOpMgr     :: !Operation.Manager
  , _clientConn      :: !Connection
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
startListening mainBus = do
  opMgr <- Operation.new
  servingFork mainBus opMgr . connectionSettings =<< reactSettings

--------------------------------------------------------------------------------
new :: Bus Settings -> Lambda Settings ()
new mainBus = do
  configure mainBus (app mainBus)

--------------------------------------------------------------------------------
clientApp :: ClientSocket -> Configure Settings ()
clientApp self = do
  subscribe (onPackageArrived self)
  subscribe (onConnectionClosed self)
  subscribe (onOperationResp self)
  subscribe (onTick self)

  timer Tick 0.2 Undefinitely

  appStart $
    do clientId <- reactSelfId
       _ <- async (processingIncomingPackage self)
       _ <- async (processingOutgoingPackage self)
       logInfo [i|New connection #{clientId} on #{_clientAddr self}|]

--------------------------------------------------------------------------------
newClientSocket :: Operation.Manager
                -> Connection
                -> SockAddr
                -> Configure s ClientSocket
newClientSocket mgr conn addr =
  ClientSocket mgr conn addr
    <$> newStopwatch
    <*> (liftIO $ newTBMQueueIO 500)
    <*> newIORef 0
    <*> newIORef initHealthTracking
    <*> newIORef False

--------------------------------------------------------------------------------
servingFork :: Bus Settings
            -> Operation.Manager
            -> ConnectionSettings
            -> React Settings ()
servingFork mainBus opMgr ConnectionSettings{..} = void $ fork $ liftBaseWith $ \run -> do
  ctx   <- initConnectionContext

  serve (Host hostname) (show portNumber) $ \(sock, addr) -> run $ reactLambda $
    do conn <- liftIO $ connectFromSocket ctx sock connectionParams
       child <- busNewChild mainBus
       configure child (clientConf conn addr)
       busProcessedEverything child
  where
    clientConf sock addr =
      newClientSocket opMgr sock addr >>= clientApp

    connectionParams =
      ConnectionParams hostname portNumber Nothing Nothing

--------------------------------------------------------------------------------
processingIncomingPackage :: ClientSocket -> React Settings ()
processingIncomingPackage ClientSocket{..} =
  handleAny onError $ forever $ do
    prefixBytes <- liftIO $ connectionGetExact _clientConn 4
    case decode prefixBytes of
      Left _ -> throwString [i|Wrong package framing.|]
      Right (PkgPrefix len) -> do
        payload <- liftIO $ connectionGetExact _clientConn (fromIntegral len)
        case decode payload of
          Left e    -> throwString [i|Package parsing error #{e}.|]
          Right pkg -> publish (PackageArrived pkg)

--------------------------------------------------------------------------------
processingOutgoingPackage :: ClientSocket -> React Settings ()
processingOutgoingPackage ClientSocket{..} = handleAny onError loop
  where
    loop = do
      msg <- atomically $ readTBMQueue _clientQueue
      for_ msg $ \pkg ->
        do liftIO $ connectionPut _clientConn (encode pkg)
           loop

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
onPackageArrived self@ClientSocket{..} (PackageArrived pkg@Pkg{..}) = do
  incrPkgNum self
  logDebug [i|Package #{pkgId} arrived.|]

  case pkgCmd of
    0x01 -> enqueuePkg self (heartbeatResponse pkgId)
    0x02 -> return ()
    _    ->
      case parseOp pkg of
        Nothing -> logError [i|Wrong operation format on #{pkg}.|]
        Just (SomeOperation op) -> do
          logDebug "Received new operation."
          handleOperation self op

--------------------------------------------------------------------------------
onOperationResp :: ClientSocket -> Operation.Resp -> React Settings ()
onOperationResp self (Operation.Resp op resp) =
  let pkg = createRespPkg op resp
   in enqueuePkg self pkg

--------------------------------------------------------------------------------
handleOperation :: ClientSocket -> Operation a -> React Settings ()
handleOperation self op = Operation.push (_clientOpMgr self) op

--------------------------------------------------------------------------------
onError :: SomeException -> React s ()
onError e = publish (ConnectionClosed $ show e)

--------------------------------------------------------------------------------
onConnectionClosed :: ClientSocket -> ConnectionClosed -> React Settings ()
onConnectionClosed self (ConnectionClosed reason) =
  closeConnection self reason
