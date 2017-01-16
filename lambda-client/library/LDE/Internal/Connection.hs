{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : LDE.Internal.Connection
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module LDE.Internal.Connection
    ( InternalConnection
    , ConnectionException(..)
    , connUUID
    , connClose
    , connSend
    , connRecv
    , connIsClosed
    , newConnection
    ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Serialize
import Data.UUID
import Data.UUID.V4
import Network.Connection
import Protocol.Package

--------------------------------------------------------------------------------
import LDE.Internal.Settings

--------------------------------------------------------------------------------
-- | Type of connection issue that can arise during the communication with the
--   server.
data ConnectionException
  = MaxAttemptConnectionReached
    -- ^ The max reconnection attempt threshold has been reached.
  | ClosedConnection
    -- ^ Use of a close 'Connection'.
  | WrongPackageFraming
    -- ^ TCP package sent by the server had a wrong framing.
  | PackageParsingError String
    -- ^ Server sent a malformed TCP package.
  deriving Show

--------------------------------------------------------------------------------
instance Exception ConnectionException

--------------------------------------------------------------------------------
data In a where
  Id    :: In UUID
  Close :: In ()
  Send  :: Pkg -> In ()
  Recv  :: In Pkg

--------------------------------------------------------------------------------
-- | Represents connection logic action to carry out.
data Status a where
  Noop             :: Status ()
  WithConnection   :: UUID -> Connection -> In a -> Status a
  CreateConnection :: In a -> Status a
  Errored          :: ConnectionException -> Status a

--------------------------------------------------------------------------------
-- | Internal representation of a connection with the server.
data InternalConnection =
  InternalConnection
  { _var   :: TMVar State
  , _setts :: Settings
  , _ctx   :: ConnectionContext
  }

--------------------------------------------------------------------------------
data State
  = Offline
  | Online UUID Connection
  | Closed

--------------------------------------------------------------------------------
-- | Creates a new 'InternalConnection'.
newConnection :: Settings -> IO InternalConnection
newConnection setts = do
  ctx <- initConnectionContext
  var <- newTMVarIO Offline
  return $ InternalConnection var setts ctx

--------------------------------------------------------------------------------
-- | Gets current 'InternalConnection' 'UUID'.
connUUID :: InternalConnection -> IO UUID
connUUID conn = execute conn Id

--------------------------------------------------------------------------------
-- | Closes the 'InternalConnection'. It will not retry to reconnect after that
--   call. it means a new 'InternalConnection' has to be created.
--   'ClosedConnection' exception will be raised if the same
--   'InternalConnection' object is used after a 'connClose' call.
connClose :: InternalConnection -> IO ()
connClose conn = execute conn Close

--------------------------------------------------------------------------------
-- | Sends 'Package' to the server.
connSend :: InternalConnection -> Pkg -> IO ()
connSend conn pkg = execute conn (Send pkg)

--------------------------------------------------------------------------------
-- | Asks the requested amount of bytes from the 'handle'.
connRecv :: InternalConnection -> IO Pkg
connRecv conn = execute conn Recv

--------------------------------------------------------------------------------
-- | Returns True if the connection is in closed state.
connIsClosed :: InternalConnection -> STM Bool
connIsClosed InternalConnection{..} = do
  r <- readTMVar _var
  case r of
    Closed -> return True
    _      -> return False

--------------------------------------------------------------------------------
-- Connection Logic
--------------------------------------------------------------------------------
onlineLogic :: forall a. TMVar State
            -> UUID
            -> Connection
            -> In a
            -> STM (Status a)
onlineLogic var uuid conn input =
  let status = WithConnection uuid conn input
      state =
        case input of
          Close -> Closed
          _     -> Online uuid conn in
  status <$ putTMVar var state

--------------------------------------------------------------------------------
offlineLogic :: forall a. TMVar State -> In a -> STM (Status a)
offlineLogic var Close = Noop <$ putTMVar var Closed
offlineLogic _ other   = return $ CreateConnection other

--------------------------------------------------------------------------------
closedLogic :: forall a. TMVar State -> In a -> STM (Status a)
closedLogic var input = do
  putTMVar var Closed
  case input of
    Close -> return Noop
    _     -> return $ Errored ClosedConnection

--------------------------------------------------------------------------------
connectionLogic :: forall a. TMVar State -> In a -> STM (Status a)
connectionLogic var input = do
  state <- takeTMVar var
  case state of
    Online uuid conn -> onlineLogic var uuid conn input
    Offline          -> offlineLogic var input
    Closed           -> closedLogic var input

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
handleInput :: forall a. UUID -> Connection -> In a -> IO a
handleInput _ conn (Send pkg) = send conn pkg
handleInput _ conn Recv       = recv conn
handleInput uuid _ Id         = return uuid
handleInput _ conn Close      = liftIO $ connectionClose conn

--------------------------------------------------------------------------------
-- | Main connection logic. It will automatically reconnect to the server when
--   a exception occured while the 'Handle' is accessed.
execute :: forall a. InternalConnection -> In a -> IO a
execute iconn input = do
  res <- atomically $ connectionLogic (_var iconn) input

  case res of
    Noop -> return ()
    Errored e -> throwIO e
    WithConnection uuid conn op -> handleInput uuid conn op
    CreateConnection op -> do
      (uuid, conn) <- openConnection iconn

      atomically $ putTMVar (_var iconn) (Online uuid conn)
      handleInput uuid conn op

--------------------------------------------------------------------------------
reachedMaxAttempt :: Retry -> Int -> Bool
reachedMaxAttempt KeepRetrying _ = False
reachedMaxAttempt (AtMost n) cur = n <= cur

--------------------------------------------------------------------------------
openConnection :: InternalConnection -> IO (UUID, Connection)
openConnection InternalConnection{..} = attempt 1
  where
    delay = s_reconnect_delay_secs _setts * secs

    handleFailure trialCount = do
      threadDelay delay

      when (reachedMaxAttempt (s_retry _setts) trialCount) $ do
        atomically $ putTMVar _var Closed
        throwIO MaxAttemptConnectionReached

      attempt (trialCount + 1)

    attempt trialCount =
      case s_connection _setts  of
        Static host port -> do
          res <- tryAny $ connect _setts _ctx host port
          case res of
            Left _   -> handleFailure trialCount
            Right st -> return st

--------------------------------------------------------------------------------
secs :: Int
secs = 1000000

--------------------------------------------------------------------------------
connect :: Settings
        -> ConnectionContext
        -> String
        -> Int
        -> IO (UUID, Connection)
connect sett ctx host port = do
  let params = ConnectionParams host (fromIntegral port) Nothing Nothing
  conn <- connectTo ctx params
  uuid <- nextRandom
  return (uuid, conn)

--------------------------------------------------------------------------------
-- Binary operations
--------------------------------------------------------------------------------
recv :: Connection -> IO Pkg
recv con = do
  header <- connectionGetExact con 4
  case decode header of
    Left _       -> throwIO WrongPackageFraming
    Right prefix -> do
      bs <- connectionGetExact con prefix
      case decode bs of
        Left e    -> throwIO $ PackageParsingError e
        Right pkg -> return pkg

--------------------------------------------------------------------------------
send :: Connection -> Pkg -> IO ()
send  con pkg = connectionPut con (encode pkg)
