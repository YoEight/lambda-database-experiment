{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RecordWildCards            #-}
--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Bus.Builder
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Bus.Builder where

--------------------------------------------------------------------------------
import Data.IORef
import Data.Typeable

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Time

--------------------------------------------------------------------------------
import Lambda.Bus.Types

--------------------------------------------------------------------------------
newtype InitT p m a =
  InitT (StateT [Callback (HandlerT p m)] IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadState [Callback (HandlerT p m)]
           )

--------------------------------------------------------------------------------
data AppState p m =
  AppState
  { _appInit :: !(InitT p m ()) }

--------------------------------------------------------------------------------
subscribe :: (Typeable a, PubSub m p) => (a -> HandlerT p m ()) -> InitT p m ()
subscribe k = modify (Callback Proxy k:)

--------------------------------------------------------------------------------
newtype HandlerT p m a =
  HandlerT (ReaderT p m a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadIO
           )

--------------------------------------------------------------------------------
publish :: (Typeable a, PubSub m p, MonadIO m) => a -> HandlerT p m ()
publish a = HandlerT $ do
  p <- ask
  _ <- liftIO $ atomically $ publishSTM p a
  return ()

--------------------------------------------------------------------------------
newtype Configure p m a =
  Configure (State (AppState p m) a)
  deriving ( Functor
           , Applicative
           , Monad
           )

--------------------------------------------------------------------------------
initialize :: InitT p m () -> Configure p m ()
initialize action = 
  Configure $ modify $ \s -> s { _appInit = _appInit s >> action }

--------------------------------------------------------------------------------
data RegisterTimer =
  forall e. Typeable e => RegisterTimer e NominalDiffTime Bool

--------------------------------------------------------------------------------
data TimerState =
  TimerState
  { _timerStopped :: IORef Bool }

--------------------------------------------------------------------------------
configureTimer :: (PubSub m p, MonadIO m) => Configure p m ()
configureTimer = initialize go
  where
    go = do
      self <- TimerState <$> liftIO (newIORef False)
      subscribe (onRegisterTimer self)

--------------------------------------------------------------------------------
onRegisterTimer :: MonadIO m => TimerState -> RegisterTimer -> HandlerT p m ()
onRegisterTimer self (RegisterTimer evt duration oneOff) =
  delayed self evt duration oneOff

--------------------------------------------------------------------------------
delayed :: (Typeable e, MonadIO m)
        => TimerState
        -> e
        -> NominalDiffTime
        -> Bool
        -> HandlerT p m ()
delayed TimerState{..} msg timespan oneOff = void $ liftIO $ forkIO loop
  where
    s2mcs = 10^6
    micros = truncate (timespan * s2mcs)
    loop = do
      liftIO $ threadDelay micros
      publish msg
      stopped <- readIORef _timerStopped
      unless (oneOff || stopped) loop

