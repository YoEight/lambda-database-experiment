{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
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
import Data.Typeable

--------------------------------------------------------------------------------
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.IORef.Lifted
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
  HandlerT { unHandlerT :: ReaderT p m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadIO
           )

--------------------------------------------------------------------------------
instance MonadTransControl (HandlerT p) where
  type StT (HandlerT p) a = a

  liftWith = defaultLiftWith HandlerT unHandlerT
  restoreT = defaultRestoreT HandlerT


--------------------------------------------------------------------------------
instance MonadBaseControl b m => MonadBaseControl b (HandlerT p m) where
  type StM (HandlerT p m) a = ComposeSt (HandlerT p) m a

  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

--------------------------------------------------------------------------------
instance MonadBase b m => MonadBase b (HandlerT p m) where
    liftBase = HandlerT . liftBase

--------------------------------------------------------------------------------
publish :: (Typeable a, PubSub m p, MonadBase IO m) => a -> HandlerT p m ()
publish a = HandlerT $ do
  p <- ask
  _ <- liftBase $ atomically $ publishSTM p a
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
configureTimer :: (PubSub m p, MonadBaseControl IO m) => Configure p m ()
configureTimer = initialize go
  where
    go = do
      self <- TimerState <$> liftIO (newIORef False)
      subscribe (onRegisterTimer self)

--------------------------------------------------------------------------------
onRegisterTimer :: (MonadBaseControl IO m, PubSub m p)
                => TimerState
                -> RegisterTimer
                -> HandlerT p m ()
onRegisterTimer self (RegisterTimer evt duration oneOff) =
  delayed self evt duration oneOff

--------------------------------------------------------------------------------
delayed :: (Typeable e, MonadBaseControl IO m, PubSub m p)
        => TimerState
        -> e
        -> NominalDiffTime
        -> Bool
        -> HandlerT p m ()
delayed TimerState{..} msg timespan oneOff = void $ fork loop
  where
    s2mcs = 10^(6 :: Int)
    micros = truncate (timespan * s2mcs)
    loop = do
      threadDelay micros
      publish msg
      stopped <- readIORef _timerStopped
      unless (oneOff || stopped) loop

