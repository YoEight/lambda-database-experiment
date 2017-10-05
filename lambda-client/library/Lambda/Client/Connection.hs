--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Client.Connection
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Lambda.Client.Connection where

--------------------------------------------------------------------------------
import Lambda.Bus
import Lambda.Prelude
import Lambda.Prelude.Stopwatch
import Protocol.Package

--------------------------------------------------------------------------------
import           Lambda.Client.EndPoint
import           Lambda.Client.Messages
import qualified Lambda.Client.Operation as Operation
import           Lambda.Client.Settings
import           Lambda.Client.TcpConnection

--------------------------------------------------------------------------------
data Attempt =
  Attempt { attemptCount    :: !Int
          , attemptLastTime :: !NominalDiffTime
          }

--------------------------------------------------------------------------------
freshAttempt :: Internal -> React Settings Attempt
freshAttempt Internal{..} = Attempt 1 <$> stopwatchElapsed _stopwatch

--------------------------------------------------------------------------------
data ConnectingState
  = Reconnecting
  | ConnectionEstablishing TcpConnection

--------------------------------------------------------------------------------
data Stage
  = Init
  | Connecting Attempt ConnectingState
  | Connected TcpConnection
  | Closed

--------------------------------------------------------------------------------
whenInit :: Internal -> React Settings () -> React Settings ()
whenInit Internal{..} m =
  readIORef _stageRef >>= \case
    Init -> m
    _    -> pure ()

--------------------------------------------------------------------------------
whenReconnecting :: Internal
                 -> (Attempt -> React Settings ())
                 -> React Settings ()
whenReconnecting Internal{..} k =
  readIORef _stageRef >>= \case
    Connecting att Reconnecting -> k att
    _                           -> pure ()

--------------------------------------------------------------------------------
whenConnectionEstablishing :: Internal
                           -> (Attempt -> TcpConnection -> React Settings ())
                           -> React Settings ()
whenConnectionEstablishing Internal{..} k =
  readIORef _stageRef >>= \case
    Connecting att (ConnectionEstablishing conn) -> k att conn
    _                                            -> pure ()

--------------------------------------------------------------------------------
whenConnectionAvalaible :: Internal
                        -> (TcpConnection -> React Settings ())
                        -> React Settings ()
whenConnectionAvalaible Internal{..} k =
  readIORef _stageRef >>= \case
    Connecting _ (ConnectionEstablishing conn) -> k conn
    Connected conn                             -> k conn
    _                                          -> pure ()

--------------------------------------------------------------------------------
switchToReconnecting :: Internal -> Int -> React Settings ()
switchToReconnecting Internal{..} tries = do
  elapsed <- stopwatchElapsed _stopwatch
  let att = Attempt tries elapsed
  atomicWriteIORef _stageRef (Connecting att Reconnecting)

--------------------------------------------------------------------------------
switchToConnectionEstablishing :: Internal
                               -> Attempt
                               -> TcpConnection
                               -> React Settings ()
switchToConnectionEstablishing Internal{..} att conn = do
  atomicWriteIORef _stageRef (Connecting att (ConnectionEstablishing conn))

--------------------------------------------------------------------------------
switchToConnected :: Internal -> TcpConnection -> React Settings ()
switchToConnected Internal{..} conn =
  atomicWriteIORef _stageRef (Connected conn)

--------------------------------------------------------------------------------
switchToClosed :: Internal -> React Settings ()
switchToClosed Internal{..} = atomicWriteIORef _stageRef Closed

--------------------------------------------------------------------------------
data Internal =
  Internal { _builder   :: ConnectionBuilder
           , _stageRef  :: IORef Stage
           , _ops       :: Operation.Manager
           , _stopwatch :: Stopwatch
           }

--------------------------------------------------------------------------------
data Tick = Tick
data Start = Start

--------------------------------------------------------------------------------
app :: ConnectionBuilder -> Configure Settings ()
app builder = do
  ref  <- newIORef Init
  let connRef = ConnectionRef $
        do stage <- readIORef ref
           case stage of
             Connecting _ (ConnectionEstablishing conn) -> pure (Just conn)
             Connected conn                             -> pure (Just conn)
             _                                          -> pure Nothing

  self <- Internal builder ref <$> Operation.new connRef
                               <*> newStopwatch

  timer Tick 0.2 Undefinitely

  subscribe (onEstablished self)
  subscribe (onConnectionError self)
  subscribe (onPackageArrived self)
  subscribe (onTick self)
  subscribe (onNewRequest self)

  appStart $ startConnecting self

--------------------------------------------------------------------------------
onEstablished :: Internal -> ConnectionEstablished -> React Settings ()
onEstablished self (ConnectionEstablished conn) = established self conn

--------------------------------------------------------------------------------
onConnectionError :: Internal -> ConnectionClosed -> React Settings ()
onConnectionError self (ConnectionClosed target cause) =
  whenConnectionAvalaible self $ \conn ->
    when (conn == target) $
      closeTcpConnection self cause conn

--------------------------------------------------------------------------------
onPackageArrived :: Internal -> PkgArrived -> React Settings ()
onPackageArrived self (PkgArrived sender pkg) = packageArrived self sender pkg

--------------------------------------------------------------------------------
onTick :: Internal -> Tick -> React Settings ()
onTick self@Internal{..} _ = readIORef _stageRef >>= \case
  Connecting att state
    | onGoingConnection state ->
      do elapsed <- stopwatchElapsed _stopwatch
         timeout <- connectionTimeout <$> reactSettings

         unless (elapsed - attemptLastTime att < timeout) $
           do let retries = attemptCount att + 1
              switchToReconnecting self retries
              logDebug [i|Checking reconnection... (attempt #{retries}).|]
              connecting self
    | otherwise -> pure ()

  Connected{} -> Operation.tick _ops

  _ -> pure ()
  where
    onGoingConnection Reconnecting             = True
    onGoingConnection ConnectionEstablishing{} = True

--------------------------------------------------------------------------------
startConnecting :: Internal -> React Settings ()
startConnecting self = whenInit self $ do
  switchToReconnecting self 1
  connecting self

--------------------------------------------------------------------------------
connecting :: Internal -> React Settings ()
connecting self@Internal{..} = whenReconnecting self $ \att -> do
  setts <- reactSettings
  let endpoint =
        case connectionType setts of
          Static host port -> EndPoint host port

  conn <- connect _builder endpoint
  switchToConnectionEstablishing self att conn

--------------------------------------------------------------------------------
established :: Internal -> TcpConnection -> React Settings ()
established self@Internal{..} conn =
  whenConnectionEstablishing self $ \_ expected ->
    when (conn == expected) $
      do logDebug [i|TCP connection established #{conn}.|]
         switchToConnected self conn

--------------------------------------------------------------------------------
packageArrived :: Internal -> TcpConnection -> Pkg -> React Settings ()
packageArrived self sender pkg =
  whenConnectionAvalaible self $ \known ->
    when (known == sender) $
      case pkgCmd pkg of
        0x01 ->
          let newPkg = pkg { pkgCmd = 0x02 }
           in enqueuePkg sender newPkg
        _ -> Operation.arrived (_ops self) pkg

--------------------------------------------------------------------------------
closeTcpConnection :: Internal
                   -> SomeException
                   -> TcpConnection
                   -> React Settings ()
closeTcpConnection self@Internal{..} cause conn = do
  logDebug [i|closeTcpConnection: connection #{conn}. Cause: #{cause}.|]
  dispose conn
  logDebug [i|closeTcpConnection: connection #{conn} disposed.|]

  readIORef _stageRef >>= \case
    Closed -> pure ()
    stage  ->
      do att <-
           case stage of
             Connecting old _ -> pure old
             _                -> freshAttempt self

         atomicWriteIORef _stageRef (Connecting att Reconnecting)

--------------------------------------------------------------------------------
onNewRequest :: Internal -> NewRequest -> React Settings ()
onNewRequest Internal{..} request = Operation.submit _ops request

