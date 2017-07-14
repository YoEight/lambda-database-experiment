{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Node.Types
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Types where

--------------------------------------------------------------------------------
import Control.Monad.Fix
import Data.Typeable
import Data.Typeable.Internal

--------------------------------------------------------------------------------
import Lambda.Node.Logger
import Lambda.Node.Monitoring
import Lambda.Node.Prelude
import Lambda.Node.Settings

--------------------------------------------------------------------------------
-- Messaging
--------------------------------------------------------------------------------
data Message where
  Message :: Typeable a => a -> Message
  deriving Typeable

--------------------------------------------------------------------------------
instance Show Message where
  show (Message a) = "Message: " <> show (typeOf a)

--------------------------------------------------------------------------------
toMsg :: Typeable a => a -> Message
toMsg = Message

--------------------------------------------------------------------------------
fromMsg :: Typeable a => Message -> Maybe a
fromMsg (Message a) = cast a

--------------------------------------------------------------------------------
newtype Session = Session UUID deriving (Eq, Ord, Hashable)

--------------------------------------------------------------------------------
newSession :: MonadIO m => m Session
newSession = Session <$> freshUUID

--------------------------------------------------------------------------------
instance Show Session where
  show (Session sid) = show sid

--------------------------------------------------------------------------------
data Callback where
  Callback :: Typeable a
           => Session
           -> Proxy a
           -> (a -> Server ())
           -> Callback

--------------------------------------------------------------------------------
instance Show Callback where
  show (Callback sid prx _) = [i|Session [#{sid}] expects #{typeRep prx}|]

--------------------------------------------------------------------------------
class PubSub p where
  subscribeSTM          :: p -> Callback -> STM ()
  publishSTM            :: Typeable a => p -> a -> STM Bool
  unsubscribeSessionSTM :: p -> Session -> STM ()

--------------------------------------------------------------------------------
subscribe :: (Typeable a, PubSub p, MonadIO m)
          => p
          -> Session
          -> (a -> Server ())
          -> m ()
subscribe p s k = atomically $ subscribeSTM p (Callback s Proxy k)

--------------------------------------------------------------------------------
publish :: (Typeable a, PubSub p, MonadIO m) => p -> a -> m ()
publish p a = atomically $ void $ publishSTM p a

--------------------------------------------------------------------------------
unsubscribeSession :: (PubSub p, MonadIO m) => p -> Session -> m ()
unsubscribeSession p s = atomically $ unsubscribeSessionSTM p s

--------------------------------------------------------------------------------
data Hub = forall h. PubSub h => Hub h

--------------------------------------------------------------------------------
instance PubSub Hub where
  subscribeSTM (Hub h)          = subscribeSTM h
  publishSTM (Hub h)            = publishSTM h
  unsubscribeSessionSTM (Hub h) = unsubscribeSessionSTM h

--------------------------------------------------------------------------------
asHub :: PubSub h => h -> Hub
asHub = Hub

--------------------------------------------------------------------------------
data Type = Type TypeRep Fingerprint

--------------------------------------------------------------------------------
instance Show Type where
  show (Type rep _) = "type " <> show rep

--------------------------------------------------------------------------------
instance Eq Type where
  Type _ a == Type _ b = a == b

--------------------------------------------------------------------------------
instance Ord Type where
  compare (Type _ a) (Type _ b) = compare a b

--------------------------------------------------------------------------------
instance Hashable Type where
  hashWithSalt s (Type _ (Fingerprint b l)) = hashWithSalt s (b, l)

--------------------------------------------------------------------------------
data GetType
  = forall a. Typeable a => FromTypeable a
  | forall prx a. Typeable a => FromProxy (prx a)

--------------------------------------------------------------------------------
getType :: GetType -> Type
getType op = Type t (typeRepFingerprint t)
  where
    t = case op of
          FromTypeable a -> typeOf a
          FromProxy prx  -> typeRep prx

--------------------------------------------------------------------------------
data Env =
  Env
  { _envHub        :: Hub
  , _envSettings   :: Settings
  , _envLogger     :: LoggerRef
  , _envMonitoring :: Monitoring
  }

--------------------------------------------------------------------------------
newtype Server a =
  Server { unServer :: ReaderT Env IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadThrow
           , MonadCatch
           , MonadIO
           , MonadFix
           )

--------------------------------------------------------------------------------
instance MonadBase IO Server where
  liftBase m = Server $ liftBase m

--------------------------------------------------------------------------------
instance MonadBaseControl IO Server where
    type StM Server a = a
    liftBaseWith run = Server $ do
      env <- ask
      s   <- liftIO $ run (\m -> runReaderT (unServer m) env)
      restoreM s
    restoreM = return

--------------------------------------------------------------------------------
instance MonadLogger Server where
  monadLoggerLog loc src lvl msg  = do
    loggerRef <- _envLogger <$> getEnv
    liftIO $ loggerCallback loggerRef loc src lvl (toLogStr msg)

--------------------------------------------------------------------------------
instance MonadLoggerIO Server where
  askLoggerIO = do
    loggerRef <- _envLogger <$> getEnv
    return (loggerCallback loggerRef)

--------------------------------------------------------------------------------
getEnv :: Server Env
getEnv = Server ask

--------------------------------------------------------------------------------
getSettings :: Server Settings
getSettings = _envSettings <$> getEnv

--------------------------------------------------------------------------------
getMonitoring :: Server Monitoring
getMonitoring = _envMonitoring <$> getEnv

--------------------------------------------------------------------------------
publishWith :: (PubSub p, Typeable a, MonadIO m) => p -> a -> m ()
publishWith p a = atomically $ do
  _ <- publishSTM p a
  return ()

--------------------------------------------------------------------------------
runServer :: PubSub p
          => LoggerRef
          -> Settings
          -> Monitoring
          -> p
          -> Server a
          -> IO a
runServer ref setts m pub (Server action) =
  runReaderT action (Env (asHub pub) setts ref m)