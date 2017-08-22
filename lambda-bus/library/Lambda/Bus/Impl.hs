{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Bus.Impl
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Lambda.Bus.Impl where

--------------------------------------------------------------------------------
import Data.Typeable

--------------------------------------------------------------------------------
import Lambda.Logger
import Lambda.Prelude

--------------------------------------------------------------------------------
import Lambda.Bus.Builder
import Lambda.Bus.Timer
import Lambda.Bus.Types

--------------------------------------------------------------------------------
type Callbacks settings = HashMap Type (Seq (Callback settings))

--------------------------------------------------------------------------------
data Bus settings =
  Bus { _busEventHandlers  :: TVar (Callbacks settings)
      , _busQueue          :: TBMQueue Message
      , _busParent         :: IORef (Maybe (Bus settings))
      , _workerAsync       :: Async ()
      }

--------------------------------------------------------------------------------
busStop :: Bus settings -> Lambda settings ()
busStop Bus{..} = atomically $ closeTBMQueue _busQueue

--------------------------------------------------------------------------------
busParent :: Bus settings -> Bus settings -> Lambda settings ()
busParent Bus{..} parent = atomicWriteIORef _busParent (Just parent)

--------------------------------------------------------------------------------
busProcessedEverything :: Bus settings -> Lambda settings ()
busProcessedEverything Bus{..} = waitAsync _workerAsync

--------------------------------------------------------------------------------
newBus :: Lambda settings (Bus settings)
newBus =
  mfix $ \self -> do
    configure self configureTimer

    Bus <$> (liftIO $ newTVarIO mempty)
        <*> (liftIO $ newTBMQueueIO 500)
        <*> newIORef Nothing
        <*> async (worker self)

--------------------------------------------------------------------------------
configure :: Bus settings -> Configure settings () -> Lambda settings ()
configure self conf = do
  atomically . traverse_ (subscribeSTM self) =<< produceCallbacks app
  traverse_ registering (_appTimers app)
  where app = runConfigure conf

        registering (Timer evt timespan plan) =
          registerTimer self evt timespan plan

--------------------------------------------------------------------------------
worker :: Bus settings -> Lambda settings ()
worker self@Bus{..} = loop
  where
    handleMsg (Message a) = do
      callbacks <- atomically $ readTVar _busEventHandlers
      publishing self callbacks a
      loop

    loop = traverse_ handleMsg =<< atomically (readTBMQueue _busQueue)

--------------------------------------------------------------------------------
instance PubSub Bus where
  subscribeSTM Bus{..} hdl@(Callback prx _) =
    modifyTVar' _busEventHandlers update
      where update callbacks =
              let
                  tpe  = getType (FromProxy prx)

                  next = alterMap $ \input ->
                    case input of
                      Nothing ->
                        Just (singleton hdl)

                      Just hs ->
                        Just (snoc hs hdl)
              in
                 next tpe callbacks

  publishSTM Bus{..} a =
    do closed <- isClosedTBMQueue _busQueue
       writeTBMQueue _busQueue (toMsg a)
       return $ not closed

--------------------------------------------------------------------------------
publishing :: Typeable a
           => Bus settings
           -> Callbacks settings
           -> a
           -> Lambda settings ()
publishing self@Bus{..} callbacks a = do
  let tpe      = getType (FromTypeable a)
      handlers = lookup tpe callbacks
  logDebug [i|Publishing message #{tpe}.|]
  traverse_ (propagate self a) handlers

  -- If there is no handlers this type of event, we try to dispatch it to its
  -- parent bus, if any.
  unless (isJust handlers) $
    do parentM <- readIORef _busParent
       for_ parentM $ \parent ->
         void $ atomically $ publishSTM parent a

  logDebug [i|Message #{tpe} propagated.|]

  unless (tpe == messageType) $
    traverse_ (propagate self (toMsg a)) (lookup messageType callbacks)

--------------------------------------------------------------------------------
propagate :: Typeable a => Bus s -> a -> Seq (Callback s) -> Lambda s ()
propagate self a =traverse_ $ \(Callback _ k) -> do
  let Just b = cast a
      tpe    = typeOf b
  outcome <- tryAny $ runReact (k b) self
  case outcome of
    Right _ -> return ()
    Left e  -> logError [i|Exception when propagating #{tpe}: #{e}.|]
