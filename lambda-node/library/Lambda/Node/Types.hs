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
           => Proxy a
           -> (a -> Server ())
           -> Callback

--------------------------------------------------------------------------------
instance Show Callback where
  show (Callback prx _) = [i|Callback expects #{typeRep prx}|]

--------------------------------------------------------------------------------
class PubSub p where
  subscribeSTM :: p -> Callback -> STM ()
  publishSTM   :: Typeable a => p -> a -> STM Bool

--------------------------------------------------------------------------------
subscribe :: (Typeable a, PubSub p, MonadIO m)
          => p
          -> (a -> Server ())
          -> m ()
subscribe p k = atomically $ subscribeSTM p (Callback Proxy k)

--------------------------------------------------------------------------------
publish :: (Typeable a, PubSub p, MonadIO m) => p -> a -> m ()
publish p a = atomically $ void $ publishSTM p a

--------------------------------------------------------------------------------
data Hub = forall h. PubSub h => Hub h

--------------------------------------------------------------------------------
instance PubSub Hub where
  subscribeSTM (Hub h) = subscribeSTM h
  publishSTM (Hub h)   = publishSTM h

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
messageType :: Type
messageType = getType (FromProxy (Proxy :: Proxy Message))

--------------------------------------------------------------------------------
data Runtime =
  Runtime
  { _runtimeSettings   :: Settings
  , _runtimeLogger     :: LoggerRef
  , _runtimeMonitoring :: Monitoring
  }

--------------------------------------------------------------------------------
data Env =
  Env
  { _envHub     :: Hub
  , _envRuntime :: Runtime
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
    loggerRef <- fmap (_runtimeLogger . _envRuntime) getEnv
    liftIO $ loggerCallback loggerRef loc src lvl (toLogStr msg)

--------------------------------------------------------------------------------
instance MonadLoggerIO Server where
  askLoggerIO = do
    loggerRef <- fmap (_runtimeLogger . _envRuntime) getEnv
    return (loggerCallback loggerRef)

--------------------------------------------------------------------------------
getEnv :: Server Env
getEnv = Server ask

--------------------------------------------------------------------------------
getSettings :: Server Settings
getSettings = fmap (_runtimeSettings . _envRuntime) getEnv

--------------------------------------------------------------------------------
getMonitoring :: Server Monitoring
getMonitoring = fmap (_runtimeMonitoring . _envRuntime) getEnv

--------------------------------------------------------------------------------
publishWith :: (PubSub p, Typeable a, MonadIO m) => p -> a -> m ()
publishWith p a = atomically $ do
  _ <- publishSTM p a
  return ()

--------------------------------------------------------------------------------
runServer :: PubSub p => Runtime -> p -> Server a -> IO a
runServer runtime pub (Server action) =
  runReaderT action (Env (asHub pub) runtime)