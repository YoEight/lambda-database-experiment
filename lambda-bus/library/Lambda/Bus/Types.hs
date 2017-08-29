{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Bus.Types
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Bus.Types where

--------------------------------------------------------------------------------
import Data.Semigroup
import Data.Typeable
import GHC.Fingerprint
import Lambda.Prelude

--------------------------------------------------------------------------------
data Message =
  forall payload. Typeable payload =>
  Message { messagePayload :: !payload
          , messageSender  :: !UUID
          , messageTarget  :: !(Maybe UUID)
          }

--------------------------------------------------------------------------------
getMessageType :: Message -> Type
getMessageType (Message p _ _) = getType (FromTypeable p)

--------------------------------------------------------------------------------
instance Show Message where
  show (Message a _ _) = "Message: " <> show (typeOf a)

--------------------------------------------------------------------------------
fromMsg :: Typeable a => Message -> Maybe a
fromMsg (Message a _ _) = cast a

--------------------------------------------------------------------------------
data Callback settings where
  Callback :: Typeable a
           => Proxy a
           -> (a -> React settings ())
           -> Callback settings

--------------------------------------------------------------------------------
instance Show (Callback settings) where
  show (Callback prx _) = "Callback expects " <> show (typeRep prx)

--------------------------------------------------------------------------------
class PubSub p where
  subscribeSTM :: p s -> Callback s -> STM ()
  publishSTM   :: p s -> Message -> STM Bool
  busId        :: p s -> UUID

  toSomeBus :: p s -> SomeBus s
  toSomeBus = SomeBus

--------------------------------------------------------------------------------
data SomeBus s = forall p. PubSub p => SomeBus (p s)

--------------------------------------------------------------------------------
instance PubSub SomeBus where
  subscribeSTM (SomeBus p) c = subscribeSTM p c
  publishSTM (SomeBus p) a = publishSTM p a
  busId (SomeBus p) = busId p
  toSomeBus = id

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
data ReactEnv settings =
  ReactEnv { _reactBus    :: !(SomeBus settings)
           , _reactSelf   :: !UUID
           , _reactSender :: !(Maybe UUID)
           }

--------------------------------------------------------------------------------
newtype React settings a =
  React { unReact :: ReaderT (ReactEnv settings) (Lambda settings) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadFix
           , MonadLogger
           , MonadBase IO
           , MonadBaseControl IO
           )

--------------------------------------------------------------------------------
publish :: Typeable a => a -> React settings ()
publish a = React $ do
  ReactEnv{..} <- ask
  let msg = Message a _reactSelf Nothing
  _ <- atomically $ publishSTM _reactBus msg
  return ()

--------------------------------------------------------------------------------
respond :: Typeable a => a -> React settings ()
respond a = React $ do
  ReactEnv{..} <- ask
  let msg = Message a _reactSelf _reactSender
  _ <- atomically $ publishSTM _reactBus msg
  return ()

--------------------------------------------------------------------------------
publishOn :: (Typeable a, PubSub p)
          => p settings
          -> UUID
          -> a
          -> Lambda settings ()
publishOn p sender a = void $ atomically $ publishSTM p msg
  where
    msg = Message a sender Nothing

--------------------------------------------------------------------------------
reactSettings :: React settings settings
reactSettings = React $ lift getSettings

--------------------------------------------------------------------------------
runReact :: React s a -> ReactEnv s -> Lambda s a
runReact (React m) env = runReaderT m env

--------------------------------------------------------------------------------
reactLambda :: Lambda s a -> React s a
reactLambda m = React $ lift m

--------------------------------------------------------------------------------
reactSelfId :: React settings UUID
reactSelfId = React $ asks _reactSelf

--------------------------------------------------------------------------------
subscribe :: Typeable a => (a -> React s ()) -> Configure s ()
subscribe k = Configure $ do
  bus <- ask
  atomically $ subscribeSTM bus $ Callback Proxy k

--------------------------------------------------------------------------------
newtype Configure s a =
  Configure (ReaderT (SomeBus s) (Lambda s) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadBase IO
           , MonadBaseControl IO
           )

--------------------------------------------------------------------------------
runConfigure :: PubSub p => Configure s () -> p s -> Lambda s ()
runConfigure (Configure m) p = runReaderT m (toSomeBus p)

--------------------------------------------------------------------------------
appStart :: React settings () -> Configure settings ()
appStart action = Configure $ do
  bus <- ask
  let reactEnv =
        ReactEnv bus (busId bus) Nothing
  lift $ runReact action reactEnv

--------------------------------------------------------------------------------
configureTimer :: Configure settings ()
configureTimer = do
  self <- TimerState <$> newIORef False
  subscribe (onRegisterTimer self)

--------------------------------------------------------------------------------
timer :: Typeable a
      => a
      -> NominalDiffTime
      -> TimerPlanning
      -> Configure settings ()
timer e timespan planning = Configure $ do
  bus <- ask
  lift $ registerTimer bus e timespan planning

--------------------------------------------------------------------------------
data TimerState =
  TimerState
  { _timerStopped :: IORef Bool }

--------------------------------------------------------------------------------
onRegisterTimer :: TimerState -> RegisterTimer -> React settings ()
onRegisterTimer self (RegisterTimer evt duration oneOff) =
  delayed self evt duration oneOff

--------------------------------------------------------------------------------
data RegisterTimer =
  forall e. Typeable e => RegisterTimer e NominalDiffTime Bool

--------------------------------------------------------------------------------
data TimerPlanning = OnOff | Undefinitely

--------------------------------------------------------------------------------
registerTimer :: (Typeable evt, PubSub p)
              => p settings
              -> evt
              -> NominalDiffTime
              -> TimerPlanning
              -> Lambda settings ()
registerTimer p evt period plan =
  publishOn p (busId p) (RegisterTimer evt period boolean)
    where boolean =
            case plan of
              OnOff ->
                True
              Undefinitely ->
                False

--------------------------------------------------------------------------------
delayed :: Typeable e
        => TimerState
        -> e
        -> NominalDiffTime
        -> Bool
        -> React settings ()
delayed TimerState{..} msg timespan oneOff = void $ fork loop
  where
    micros = truncate (timespan * s2mcs)
    loop = do
      threadDelay micros
      publish msg
      stopped <- readIORef _timerStopped
      unless (oneOff || stopped) loop

