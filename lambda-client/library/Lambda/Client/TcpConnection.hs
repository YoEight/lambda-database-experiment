{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Client.TcpConnection
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- TCP connection type declarations.
--------------------------------------------------------------------------------
module Lambda.Client.TcpConnection
  ( ConnectionBuilder(..)
  , TcpConnection(..)
  , RecvOutcome(..)
  , PkgArrived(..)
  , ConnectionError(..)
  , ConnectionEstablished(..)
  , ConnectionClosed(..)
  , ConnectionRef(..)
  , getConnection
  , connectionBuilder
  , connectionError
  ) where

--------------------------------------------------------------------------------
import           Data.Serialize
import           Lambda.Bus
import           Lambda.Logger
import           Lambda.Prelude
import           Protocol.Package
import qualified Network.Connection as Network

--------------------------------------------------------------------------------
import Lambda.Client.EndPoint
import Lambda.Client.Settings

--------------------------------------------------------------------------------
-- | Utility type that knows how to create a 'TcpConnection'.
newtype ConnectionBuilder =
  ConnectionBuilder { connect :: EndPoint -> React Settings TcpConnection }

--------------------------------------------------------------------------------
-- | Represents all kind of outcome that can occur when we try to read from a
--   TCP socket.
data RecvOutcome
  = ResetByPeer
  | Recv Pkg
  | WrongFraming
  | ParsingError

---------------------------------------------------------------------------------
type ConnectionId = UUID

--------------------------------------------------------------------------------
-- | Stateful computation that knows when a 'TcpConnection' is available.
newtype ConnectionRef =
  ConnectionRef { maybeConnection :: React Settings (Maybe TcpConnection) }

--------------------------------------------------------------------------------
-- | Partial way to obtain a 'TcpConnection'. Use it when you really sure a
--   'TcpConnection' is available at that time.
getConnection :: ConnectionRef -> React Settings TcpConnection
getConnection ref =
  maybeConnection ref >>= \case
    Just conn -> return conn
    Nothing   -> do
      logError "Expected a connection but got none."
      throwString "No current connection (impossible situation)"

--------------------------------------------------------------------------------
-- | Represents a tcp connection.
data TcpConnection =
  TcpConnection { connectionId :: ConnectionId
                  -- ^ Unique connection id.
                , connectionEndPoint :: EndPoint
                  -- ^ Endpoint supported by this connection.
                , enqueuePkg :: Pkg -> React Settings ()
                  -- ^ Pushes a 'Pkg' to be sent.
                , dispose :: React Settings ()
                  -- ^ Called when a conection is disposed.
                }

--------------------------------------------------------------------------------
instance Show TcpConnection where
  show TcpConnection{..} =
    [i|"Connection #{connectionId} on show #{connectionEndPoint}.|]

--------------------------------------------------------------------------------
instance Eq TcpConnection where
  a == b = connectionId a == connectionId b

--------------------------------------------------------------------------------
-- | 'TcpConnection' internal state. So far, only closeable queue.
newtype ConnectionState =
  ConnectionState { _sendQueue :: TBMQueue Pkg }

--------------------------------------------------------------------------------
-- | Event sent when 'Pkg' has been sent by the server.
data PkgArrived = PkgArrived TcpConnection Pkg deriving Typeable

--------------------------------------------------------------------------------
-- | Event sent when a 'TcpConnection' has errored.
data ConnectionError =
  ConnectionError TcpConnection SomeException deriving Typeable

--------------------------------------------------------------------------------
-- | Smart constructor from 'ConnectionError' event.
connectionError :: Exception e => TcpConnection -> e -> ConnectionError
connectionError c = ConnectionError c . toException

--------------------------------------------------------------------------------
-- | Event sent when a connection has been closed.
data ConnectionClosed = ConnectionClosed TcpConnection SomeException
  deriving Typeable

--------------------------------------------------------------------------------
-- | Event sent when a 'TcpConnection' has been established.
data ConnectionEstablished = ConnectionEstablished TcpConnection

--------------------------------------------------------------------------------
-- | Event sent when a connection has been stopped abruptly.
newtype ConnectionResetByPeer = ConnectionResetByPeer SomeException

--------------------------------------------------------------------------------
instance Show ConnectionResetByPeer where
  show (ConnectionResetByPeer reason) =
    "Connection reset by peer: " <> show reason

--------------------------------------------------------------------------------
instance Exception ConnectionResetByPeer

--------------------------------------------------------------------------------
-- | Error raised when the communication protocol expectation hasn't been meet.
data ProtocolError
  = WrongFramingError !String
    -- ^ See https://blog.stephencleary.com/2009/04/message-framing.html
    --   for more information.
  | PkgParsingError !String
    -- Wrong 'Pkg' format.
  deriving Typeable

--------------------------------------------------------------------------------
instance Show ProtocolError where
  show (WrongFramingError reason)   = "Pkg framing error: " <> reason
  show (PkgParsingError reason) = "Pkg parsing error: " <> reason

--------------------------------------------------------------------------------
instance Exception ProtocolError

--------------------------------------------------------------------------------
-- | Creates asynchronous TCP connection.
connectionBuilder :: Lambda Settings ConnectionBuilder
connectionBuilder = do
  ctx <- liftIO $ Network.initConnectionContext
  return $ ConnectionBuilder $ \ept -> do
    cid <- freshUUID
    state <- createState

    mfix $ \self -> do
      tcpConnAsync <- async $
        tryAny (openConnection ctx ept) >>= \case
          Left e -> do
            publish (ConnectionClosed self e)
            throw e
          Right conn -> do
            publish (ConnectionEstablished self)
            return conn

      sendAsync <- async (sending state self tcpConnAsync)
      recvAsync <- async (receiving state self tcpConnAsync)
      return TcpConnection { connectionId       = cid
                           , connectionEndPoint = ept
                           , enqueuePkg     = enqueue state
                           , dispose            = do
                               closeState state
                               disposeConnection tcpConnAsync
                               cancel sendAsync
                               cancel recvAsync
                           }

--------------------------------------------------------------------------------
-- | Creates a 'TcpConnection' internal state.
createState :: React Settings ConnectionState
createState = ConnectionState <$> liftIO (newTBMQueueIO 500)

--------------------------------------------------------------------------------
-- | Closes a 'TcpConnection'.
closeState :: ConnectionState -> React Settings ()
closeState ConnectionState{..} = atomically $ closeTBMQueue _sendQueue

--------------------------------------------------------------------------------
-- | Opens a TCP connection.
openConnection :: Network.ConnectionContext
                 -> EndPoint
                 -> React Settings Network.Connection
openConnection ctx ept = liftIO $ Network.connectTo ctx params
  where
    host   = endPointIp ept
    port   = fromIntegral $ endPointPort ept
    params = Network.ConnectionParams host port Nothing Nothing

--------------------------------------------------------------------------------
-- | Closes and disposes a tcp connection.
disposeConnection :: Async Network.Connection -> React Settings ()
disposeConnection as = traverse_ tryDisposing =<< pollAsync as
  where
    tryDisposing = traverse_ disposing
    disposing    = liftIO . Network.connectionClose

--------------------------------------------------------------------------------
-- | Parses a 'Pkg' from a TCP connection.
receivePkg :: TcpConnection -> Network.Connection -> React Settings Pkg
receivePkg self conn =
  tryAny (liftIO $ Network.connectionGetExact conn 4) >>= \case
    Left e -> do
      publish (ConnectionClosed self e)
      throw e
    Right frame ->
      case runGet get frame of
        Left reason -> do
          let cause = WrongFramingError reason
          publish (connectionError self cause)
          throw cause
        Right prefix -> do
          let frameSiz = pkgPrefixIntegral prefix
          tryAny (liftIO $ Network.connectionGetExact conn frameSiz) >>= \case
            Left e -> do
              publish (ConnectionClosed self e)
              throw e
            Right payload ->
              case runGet get payload of
                Left reason -> do
                  let cause = PkgParsingError reason
                  publish (connectionError self cause)
                  throw cause
                Right pkg -> return pkg

--------------------------------------------------------------------------------
-- | Read thread worker.
receiving :: ConnectionState
          -> TcpConnection
          -> Async Network.Connection
          -> React Settings ()
receiving ConnectionState{..} self tcpConnAsync =
  forever . go =<< waitAsync tcpConnAsync
  where
    go conn =
      publish . PkgArrived self =<< receivePkg self conn

--------------------------------------------------------------------------------
-- | Adds a 'Pkg' to the connection queue.
enqueue :: ConnectionState -> Pkg -> React Settings ()
enqueue ConnectionState{..} pkg@Pkg{..} = do
  logDebug [i|Pkg enqueued: #{pkg}|]
  atomically $ writeTBMQueue _sendQueue pkg

--------------------------------------------------------------------------------
-- | Write thread worker.
sending :: ConnectionState
        -> TcpConnection
        -> Async Network.Connection
        -> React Settings ()
sending ConnectionState{..} self tcpConnAsync = go =<< waitAsync tcpConnAsync
  where
    go conn =
      let loop     = traverse_ send =<< atomically (readTBMQueue _sendQueue)
          send pkg =
            tryAny (liftIO $ Network.connectionPut conn bytes) >>= \case
              Left e  -> publish (ConnectionClosed self e)
              Right _ -> do
                -- TODO - Re-introduce monitoring metrics.
                -- monitorAddDataTransmitted (length bytes)
                loop
            where
              bytes = runPut $ put pkg in
      loop

