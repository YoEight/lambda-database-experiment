{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans

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
init :: InitT p m () -> Configure p m ()
init action = Configure $ modify $ \s -> s { _appInit = _appInit s >> action }
