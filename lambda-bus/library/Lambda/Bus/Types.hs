{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
-- Main Bus types declaration module.
--------------------------------------------------------------------------------
module Lambda.Bus.Types where

--------------------------------------------------------------------------------
import Data.Semigroup
import Data.Typeable
import GHC.Fingerprint
import Lambda.Prelude

--------------------------------------------------------------------------------
-- Message sent and received by a bus. It also contains information that helps
-- its routing.
data Message =
  forall payload. Typeable payload =>
  Message { messagePayload :: !payload
            -- ^ True message sent or received by a message bus.
          , messageSender :: !UUID
            -- ^ Who sent this message.
          , messageTarget  :: !(Maybe UUID)
            -- ^ Who is targetted by this message. If 'Nothing', then the
            -- message bus sends it to itself.
          }

--------------------------------------------------------------------------------
-- Returns the type representation of a 'Message'.
getMessageType :: Message -> Type
getMessageType (Message p _ _) = getType (FromTypeable p)

--------------------------------------------------------------------------------
instance Show Message where
  show (Message a _ _) = "Message: " <> show (typeOf a)

--------------------------------------------------------------------------------
-- Returns a typeful representation of a 'Message'. If the wrong message type is
-- asked, it returns 'Nothing'.
fromMsg :: Typeable a => Message -> Maybe a
fromMsg (Message a _ _) = cast a

--------------------------------------------------------------------------------
-- Used to correlate a message type to its reaction callback.
data Callback settings where
  Callback :: Typeable a
           => Proxy a
           -> (a -> React settings ())
           -> Callback settings

--------------------------------------------------------------------------------
instance Show (Callback settings) where
  show (Callback prx _) = "Callback expects " <> show (typeRep prx)

--------------------------------------------------------------------------------
-- Message bus abstraction.
class PubSub p where
  -- | Subscribes to a specific message.
  subscribeSTM :: p s -> Callback s -> STM ()

  -- | Publishes a message to the message bus.
  publishSTM   :: p s -> Message -> STM Bool

  -- | Returns message bus unique id.
  busId        :: p s -> UUID

  -- | Stops a message bus. After that call, a message bus can no longer
  --   accepts new subscription nor incoming message.
  busStop      :: p s -> Lambda s ()

  -- | Returns a existentially type representation of a message bus.
  toSomeBus :: p s -> SomeBus s
  toSomeBus = SomeBus

--------------------------------------------------------------------------------
-- | Parent type of all message bus.
data SomeBus s = forall p. PubSub p => SomeBus (p s)

--------------------------------------------------------------------------------
instance PubSub SomeBus where
  subscribeSTM (SomeBus p) c = subscribeSTM p c
  publishSTM (SomeBus p) a = publishSTM p a
  busId (SomeBus p) = busId p
  busStop (SomeBus p) = busStop p
  toSomeBus = id

--------------------------------------------------------------------------------
-- | A type representation helping when casting a 'Message'.
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
-- | Represents the different ways of getting a 'Type' representation.
data GetType
  = forall a. Typeable a => FromTypeable a
  | forall prx a. Typeable a => FromProxy (prx a)

--------------------------------------------------------------------------------
-- | Returns a type representation.
getType :: GetType -> Type
getType op = Type t (typeRepFingerprint t)
  where
    t = case op of
          FromTypeable a -> typeOf a
          FromProxy prx  -> typeRep prx

--------------------------------------------------------------------------------
-- | Type representation of a 'Message'.
messageType :: Type
messageType = getType (FromProxy (Proxy :: Proxy Message))

--------------------------------------------------------------------------------
-- | Environment used when serving a message to a message callback.
data ReactEnv settings =
  ReactEnv { _reactBus :: !(SomeBus settings)
             -- ^  The bus which received the message.
           , _reactSelf :: !UUID
             -- ^ Id of the bus.
           , _reactSender :: !(Maybe UUID)
             -- ^ Who sends the message. If 'Nothing', it means the bus sends
             --   to itself.
           }

--------------------------------------------------------------------------------
-- | Effect used when reacting to an incoming message.
newtype React settings a =
  React { unReact :: ReaderT (ReactEnv settings) (Lambda settings) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadFix
           , MonadThrow
           , MonadCatch
           , MonadLogger
           , MonadBase IO
           , MonadBaseControl IO
           )

--------------------------------------------------------------------------------
-- | Publishes a message to the same message queue.
publish :: Typeable a => a -> React settings ()
publish a = React $ do
  ReactEnv{..} <- ask
  let msg = Message a _reactSelf Nothing
  _ <- atomically $ publishSTM _reactBus msg
  return ()

--------------------------------------------------------------------------------
-- | Responds to the sender of of the message who triggered this reaction. If
--   there isn't a sender, it will act like 'publish'.
respond :: Typeable a => a -> React settings ()
respond a = React $ do
  ReactEnv{..} <- ask
  let msg = Message a _reactSelf _reactSender
  _ <- atomically $ publishSTM _reactBus msg
  return ()

--------------------------------------------------------------------------------
-- | Publishes a message to a specific message bus.
publishOn :: (Typeable a, PubSub p)
          => p settings -- Some bus.
          -> UUID       -- Id of the sender.
          -> a          -- message.
          -> Lambda settings ()
publishOn p sender a = void $ atomically $ publishSTM p msg
  where
    msg = Message a sender Nothing

--------------------------------------------------------------------------------
-- | Like 'publishOn' but lifted in 'React' monad.
sendTo :: (Typeable a, PubSub p) => p settings -> a -> React settings ()
sendTo bus evt = reactLambda $ publishOn bus (busId bus) evt

--------------------------------------------------------------------------------
-- | Like 'busStop' but lifted in 'React' monad.
stop :: React s ()
stop = React $ do
  bus <- asks _reactBus
  lift $ busStop bus

--------------------------------------------------------------------------------
-- | Returns the settings of this application.
reactSettings :: React settings settings
reactSettings = React $ lift getSettings

--------------------------------------------------------------------------------
-- | Runs 'React' monad with a proper environment.
runReact :: React s a -> ReactEnv s -> Lambda s a
runReact (React m) env = runReaderT m env

--------------------------------------------------------------------------------
-- | Lift a 'Lambda' computation in 'React' monad.
reactLambda :: Lambda s a -> React s a
reactLambda m = React $ lift m

--------------------------------------------------------------------------------
-- | Returns the id of the message bus.
reactSelfId :: React settings UUID
reactSelfId = React $ asks _reactSelf

--------------------------------------------------------------------------------
-- | Returns the message bus.
reactBus :: React settings (SomeBus settings)
reactBus = React $ asks _reactBus

--------------------------------------------------------------------------------
-- | Subscribes to a specific message type and uses the provided callback as a
--   reaction.
subscribe :: Typeable a => (a -> React s ()) -> Configure s ()
subscribe k = Configure $ do
  bus <- ask
  atomically $ subscribeSTM bus $ Callback Proxy k

--------------------------------------------------------------------------------
-- | Computation used to configure a message bus.
newtype Configure s a =
  Configure (ReaderT (SomeBus s) (Lambda s) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadFix
           , MonadIO
           , MonadBase IO
           , MonadBaseControl IO
           )

--------------------------------------------------------------------------------
-- | Runs 'Configure' computation upon a message bus.
runConfigure :: PubSub p => Configure s () -> p s -> Lambda s ()
runConfigure (Configure m) p = runReaderT m (toSomeBus p)

--------------------------------------------------------------------------------
-- | Action to run after `Configure` computation completes.
appStart :: React settings () -> Configure settings ()
appStart action = Configure $ do
  bus <- ask
  let reactEnv =
        ReactEnv bus (busId bus) Nothing
  lift $ runReact action reactEnv

--------------------------------------------------------------------------------
-- | Configures a timer manager so it can receive timer requests.
configureTimer :: Configure settings ()
configureTimer = do
  self <- TimerState <$> newIORef False
  subscribe (onRegisterTimer self)

--------------------------------------------------------------------------------
-- | Sends a timer request.
timer :: Typeable a
      => a               -- Message to send back when time runs out.
      -> NominalDiffTime -- Time period before the timer runs out.
      -> TimerPlanning   -- Timer strategy.
      -> Configure settings ()
timer e timespan planning = Configure $ do
  bus <- ask
  lift $ registerTimer bus e timespan planning

--------------------------------------------------------------------------------
-- | A timer state.
data TimerState =
  TimerState
  { _timerStopped :: IORef Bool }

--------------------------------------------------------------------------------
-- | What is done when timer request is sent.
onRegisterTimer :: TimerState -> RegisterTimer -> React settings ()
onRegisterTimer self (RegisterTimer evt duration oneOff) =
  delayed self evt duration oneOff

--------------------------------------------------------------------------------
-- | Timer request.
data RegisterTimer =
  forall e. Typeable e => RegisterTimer e NominalDiffTime Bool

--------------------------------------------------------------------------------
-- | Tells if a timer request is a one time thing or must be repeated
--   undefinitely.
data TimerPlanning = OnOff | Undefinitely

--------------------------------------------------------------------------------
-- | Emits a timer request.
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
-- | Action run in a response of a timer request.
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

