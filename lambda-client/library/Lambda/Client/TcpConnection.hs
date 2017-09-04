{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
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
newtype ConnectionBuilder =
  ConnectionBuilder { connect :: EndPoint -> React Settings TcpConnection }

--------------------------------------------------------------------------------
data RecvOutcome
  = ResetByPeer
  | Recv Pkg
  | WrongFraming
  | ParsingError

---------------------------------------------------------------------------------
type ConnectionId = UUID

--------------------------------------------------------------------------------
newtype ConnectionRef =
  ConnectionRef { maybeConnection :: React Settings (Maybe TcpConnection) }

--------------------------------------------------------------------------------
getConnection :: ConnectionRef -> React Settings TcpConnection
getConnection ref =
  maybeConnection ref >>= \case
    Just conn -> return conn
    Nothing   -> do
      logError "Expected a connection but got none."
      throwString "No current connection (impossible situation)"

--------------------------------------------------------------------------------
data TcpConnection =
  TcpConnection { connectionId       :: ConnectionId
                , connectionEndPoint :: EndPoint
                , enqueuePkg     :: Pkg -> React Settings ()
                , dispose            :: React Settings ()
                }

--------------------------------------------------------------------------------
instance Show TcpConnection where
  show TcpConnection{..} = "Connection [" <> show connectionId <> "] on "
                        <> show connectionEndPoint

--------------------------------------------------------------------------------
instance Eq TcpConnection where
  a == b = connectionId a == connectionId b

--------------------------------------------------------------------------------
newtype ConnectionState =
  ConnectionState { _sendQueue :: TBMQueue Pkg }

--------------------------------------------------------------------------------
data PkgArrived = PkgArrived TcpConnection Pkg deriving Typeable

--------------------------------------------------------------------------------
data ConnectionError =
  ConnectionError TcpConnection SomeException deriving Typeable

--------------------------------------------------------------------------------
connectionError :: Exception e => TcpConnection -> e -> ConnectionError
connectionError c = ConnectionError c . toException

--------------------------------------------------------------------------------
data ConnectionClosed = ConnectionClosed TcpConnection SomeException
  deriving Typeable

--------------------------------------------------------------------------------
data ConnectionEstablished = ConnectionEstablished TcpConnection

--------------------------------------------------------------------------------
newtype ConnectionResetByPeer = ConnectionResetByPeer SomeException

--------------------------------------------------------------------------------
instance Show ConnectionResetByPeer where
  show (ConnectionResetByPeer reason) =
    "Connection reset by peer: " <> show reason

--------------------------------------------------------------------------------
instance Exception ConnectionResetByPeer

--------------------------------------------------------------------------------
data ProtocolError
  = WrongFramingError !String
  | PkgParsingError !String
  deriving Typeable

--------------------------------------------------------------------------------
instance Show ProtocolError where
  show (WrongFramingError reason)   = "Pkg framing error: " <> reason
  show (PkgParsingError reason) = "Pkg parsing error: " <> reason

--------------------------------------------------------------------------------
instance Exception ProtocolError

--------------------------------------------------------------------------------
connectionBuilder :: Lambda Settings ConnectionBuilder
connectionBuilder = do
  ctx <- liftIO $ Network.initConnectionContext
  return $ ConnectionBuilder $ \ept -> do
    cid <- freshUUID
    state <- createState

    mfix $ \self -> do
      tcpConnAsync <- async $
        tryAny (createConnection ctx ept) >>= \case
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
createState :: React Settings ConnectionState
createState = ConnectionState <$> liftIO (newTBMQueueIO 500)

--------------------------------------------------------------------------------
closeState :: ConnectionState -> React Settings ()
closeState ConnectionState{..} = atomically $ closeTBMQueue _sendQueue

--------------------------------------------------------------------------------
createConnection :: Network.ConnectionContext
                 -> EndPoint
                 -> React Settings Network.Connection
createConnection ctx ept = liftIO $ Network.connectTo ctx params
  where
    host   = endPointIp ept
    port   = fromIntegral $ endPointPort ept
    params = Network.ConnectionParams host port Nothing Nothing

--------------------------------------------------------------------------------
disposeConnection :: Async Network.Connection -> React Settings ()
disposeConnection as = traverse_ tryDisposing =<< pollAsync as
  where
    tryDisposing = traverse_ disposing
    disposing    = liftIO . Network.connectionClose

--------------------------------------------------------------------------------
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
enqueue :: ConnectionState -> Pkg -> React Settings ()
enqueue ConnectionState{..} pkg@Pkg{..} = do
  logDebug [i|Pkg enqueued: #{pkg}|]
  atomically $ writeTBMQueue _sendQueue pkg

--------------------------------------------------------------------------------
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

