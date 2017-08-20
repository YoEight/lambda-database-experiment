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
import Control.Monad.State.Strict
import Lambda.Prelude

--------------------------------------------------------------------------------
import Lambda.Bus.Types

--------------------------------------------------------------------------------
newtype Init settings a =
  Init (StateT (Seq (Callback settings)) (Lambda settings) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadState (Seq (Callback settings))
           , MonadBase IO
           , MonadBaseControl IO
           )

--------------------------------------------------------------------------------
runInit :: Init s a -> Lambda s (Seq (Callback s))
runInit (Init m) = execStateT m mempty

--------------------------------------------------------------------------------
data AppState settings =
  AppState
  { _appInit :: !(Init settings ()) }

--------------------------------------------------------------------------------
produceCallbacks :: AppState s -> Lambda s (Seq (Callback s))
produceCallbacks app = runInit (_appInit app)

--------------------------------------------------------------------------------
subscribe :: Typeable a => (a -> React settings ()) -> Init settings ()
subscribe k = modify (`snoc` Callback Proxy k)

--------------------------------------------------------------------------------
newtype Configure settings a =
  Configure (State (AppState settings) a)
  deriving ( Functor
           , Applicative
           , Monad
           )

--------------------------------------------------------------------------------
runConfigure :: Configure settings a -> AppState settings
runConfigure (Configure m) = execState m initState
  where initState = AppState (return ())

--------------------------------------------------------------------------------
initialize :: Init settings () -> Configure settings ()
initialize action =
  Configure $ modify $ \s -> s { _appInit = _appInit s >> action }

