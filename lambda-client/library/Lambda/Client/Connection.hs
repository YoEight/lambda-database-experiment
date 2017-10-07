--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Client.Connection
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- TCP Connection manager. That module goal is to create, maintain and request
-- operations to the LDE database server.
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
-- | Holds connection attempt state.
data Attempt =
  Attempt { attemptCount :: !Int
            -- ^ How many times we tried to connect to the server.
          , attemptLastTime :: !NominalDiffTime
            -- ^ Since when we try to connect to the database server.
          }

--------------------------------------------------------------------------------
-- | Creates a new 'Attempt'.
freshAttempt :: Internal -> React Settings Attempt
freshAttempt Internal{..} = Attempt 1 <$> stopwatchElapsed _stopwatch

--------------------------------------------------------------------------------
-- | Connection attempt state.
data ConnectingState
  = Reconnecting
  | ConnectionEstablishing TcpConnection

--------------------------------------------------------------------------------
-- | Manager state.
data Stage
  = Init
  | Connecting Attempt ConnectingState
  | Connected TcpConnection
  | Closed

--------------------------------------------------------------------------------
-- | Performs an action if at 'Init' stage.
whenInit :: Internal -> React Settings () -> React Settings ()
whenInit Internal{..} m =
  readIORef _stageRef >>= \case
    Init -> m
    _    -> pure ()

--------------------------------------------------------------------------------
-- | Performs an action if at 'Connecting' stage and 'Reconnecting' state.
whenReconnecting :: Internal
                 -> (Attempt -> React Settings ())
                 -> React Settings ()
whenReconnecting Internal{..} k =
  readIORef _stageRef >>= \case
    Connecting att Reconnecting -> k att
    _                           -> pure ()

--------------------------------------------------------------------------------
-- | Performs an action if at 'Connecting' stage and 'ConnectionEstablishing'
--   state.
whenConnectionEstablishing :: Internal
                           -> (Attempt -> TcpConnection -> React Settings ())
                           -> React Settings ()
whenConnectionEstablishing Internal{..} k =
  readIORef _stageRef >>= \case
    Connecting att (ConnectionEstablishing conn) -> k att conn
    _                                            -> pure ()

--------------------------------------------------------------------------------
-- | Performs an action if at 'Connected' stage or 'Connecting' stage and
--   'ConnectionEstablishing' state.
whenConnectionAvalaible :: Internal
                        -> (TcpConnection -> React Settings ())
                        -> React Settings ()
whenConnectionAvalaible Internal{..} k =
  readIORef _stageRef >>= \case
    Connecting _ (ConnectionEstablishing conn) -> k conn
    Connected conn                             -> k conn
    _                                          -> pure ()

--------------------------------------------------------------------------------
-- | Switches to `Reconnecting` state.
switchToReconnecting :: Internal -> Int -> React Settings ()
switchToReconnecting Internal{..} tries = do
  elapsed <- stopwatchElapsed _stopwatch
  let att = Attempt tries elapsed
  atomicWriteIORef _stageRef (Connecting att Reconnecting)

--------------------------------------------------------------------------------
-- | Switches to 'ConnectionEstablishing' state.
switchToConnectionEstablishing :: Internal
                               -> Attempt
                               -> TcpConnection
                               -> React Settings ()
switchToConnectionEstablishing Internal{..} att conn = do
  atomicWriteIORef _stageRef (Connecting att (ConnectionEstablishing conn))

--------------------------------------------------------------------------------
-- | Switches to 'Connected' stage.
switchToConnected :: Internal -> TcpConnection -> React Settings ()
switchToConnected Internal{..} conn =
  atomicWriteIORef _stageRef (Connected conn)

--------------------------------------------------------------------------------
-- | Switches to 'Closed' stage.
switchToClosed :: Internal -> React Settings ()
switchToClosed Internal{..} = atomicWriteIORef _stageRef Closed

--------------------------------------------------------------------------------
-- | Connection manager state.
data Internal =
  Internal { _builder :: ConnectionBuilder
             -- ^ Connection build. It knows how to create a new connection to
             --   the server.
           , _stageRef :: IORef Stage
             -- ^ Stage reference.
           , _ops :: Operation.Manager
           , _stopwatch :: Stopwatch
           }

--------------------------------------------------------------------------------
-- | Tick message sends by a timer. Its goal is to keep the manager stimulated
--   so it can detects connection or operation timeouts.
data Tick = Tick

--------------------------------------------------------------------------------
-- | Connection manager application.
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
-- | Called when a connection has been established.
onEstablished :: Internal -> ConnectionEstablished -> React Settings ()
onEstablished self (ConnectionEstablished conn) = established self conn

--------------------------------------------------------------------------------
-- | Called when the current connection has closed.
onConnectionError :: Internal -> ConnectionClosed -> React Settings ()
onConnectionError self (ConnectionClosed target cause) =
  whenConnectionAvalaible self $ \conn ->
    when (conn == target) $
      closeTcpConnection self cause conn

--------------------------------------------------------------------------------
-- | Called when a 'Pkg' arrived.
onPackageArrived :: Internal -> PkgArrived -> React Settings ()
onPackageArrived self (PkgArrived sender pkg) = packageArrived self sender pkg

--------------------------------------------------------------------------------
-- | Called when timer message 'Tick' arrived. Depending of the manager it does
--   the following.
--
--   * 'Connecting': Verify if the connection attempt has timeout. If yes, it
--                   tries a new connection attempt.
--
--   * 'Connected': We let the operation manager performing its internal
--                  bookkeeping.
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
-- | First action done when the application is up.
startConnecting :: Internal -> React Settings ()
startConnecting self = whenInit self $ do
  switchToReconnecting self 1
  connecting self

--------------------------------------------------------------------------------
-- | Tries to open a 'TcpConnection'.
connecting :: Internal -> React Settings ()
connecting self@Internal{..} = whenReconnecting self $ \att -> do
  setts <- reactSettings
  let endpoint =
        case connectionType setts of
          Static host port -> EndPoint host port

  conn <- connect _builder endpoint
  switchToConnectionEstablishing self att conn

--------------------------------------------------------------------------------
-- | If the 'TcpConnection' is valid, switches to 'Connected' stage.
established :: Internal -> TcpConnection -> React Settings ()
established self@Internal{..} conn =
  whenConnectionEstablishing self $ \_ expected ->
    when (conn == expected) $
      do logDebug [i|TCP connection established #{conn}.|]
         switchToConnected self conn

--------------------------------------------------------------------------------
-- | If 'TcpConnection' which sent that 'Pkg' is the same as the one we store
--   at 'Connected' stage, we propagate the 'Pkg' to the operation manager.
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
-- | Closes a 'TcpConnection', creates or updates an 'Attempt' if the connection
--   manager stage is not 'Closed'.
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
-- | Registers a new request. It will be sent promptly to the database server.
onNewRequest :: Internal -> NewRequest -> React Settings ()
onNewRequest Internal{..} request = Operation.submit _ops request

