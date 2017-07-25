{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Client.Types
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Client.Types where

--------------------------------------------------------------------------------
import Data.Typeable
import Data.Typeable.Internal

--------------------------------------------------------------------------------
import Lambda.Client.Logger
import Lambda.Client.Prelude
import Lambda.Client.Settings

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
data Callback where
  Callback :: Typeable a
           => Proxy a
           -> (a -> Driver ())
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
          -> (a -> Driver ())
          -> m ()
subscribe p k = atomically $ subscribeSTM p (Callback Proxy k)

--------------------------------------------------------------------------------
publish :: Typeable a => a -> Driver ()
publish a = do
  h <- getHub
  atomically $ void $ publishSTM h a

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
  { _runtimeSettings :: Settings
  , _runtimeLogger   :: LoggerRef
  }

--------------------------------------------------------------------------------
data Env =
  Env
  { _envHub     :: Hub
  , _envRuntime :: Runtime
  }

--------------------------------------------------------------------------------
newtype Driver a =
  Driver { unDriver :: ReaderT Env IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadThrow
           , MonadCatch
           , MonadIO
           , MonadFix
           )

--------------------------------------------------------------------------------
instance MonadBase IO Driver where
  liftBase m = Driver $ liftBase m

--------------------------------------------------------------------------------
instance MonadBaseControl IO Driver where
    type StM Driver a = a
    liftBaseWith run = Driver $ do
      env <- ask
      s   <- liftIO $ run (\m -> runReaderT (unDriver m) env)
      restoreM s
    restoreM = return

--------------------------------------------------------------------------------
instance MonadLogger Driver where
  monadLoggerLog loc src lvl msg  = do
    loggerRef <- fmap (_runtimeLogger . _envRuntime) getEnv
    liftIO $ loggerCallback loggerRef loc src lvl (toLogStr msg)

--------------------------------------------------------------------------------
instance MonadLoggerIO Driver where
  askLoggerIO = do
    loggerRef <- fmap (_runtimeLogger . _envRuntime) getEnv
    return (loggerCallback loggerRef)

--------------------------------------------------------------------------------
getEnv :: Driver Env
getEnv = Driver ask

--------------------------------------------------------------------------------
getHub :: Driver Hub
getHub = _envHub <$> getEnv

--------------------------------------------------------------------------------
getSettings :: Driver Settings
getSettings = fmap (_runtimeSettings . _envRuntime) getEnv

--------------------------------------------------------------------------------
publishWith :: (PubSub p, Typeable a, MonadIO m) => p -> a -> m ()
publishWith p a = atomically $ do
  _ <- publishSTM p a
  return ()

--------------------------------------------------------------------------------
runDriver :: PubSub p => Runtime -> p -> Driver a -> IO a
runDriver runtime pub (Driver action) =
  runReaderT action (Env (asHub pub) runtime)
