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
import Lambda.Bus.Types

--------------------------------------------------------------------------------
type Callbacks m = HashMap Type (Seq (Callback m))

--------------------------------------------------------------------------------
data Bus m =
  Bus { _busLoggerRef      :: LoggerRef
      , _busEventHandlers  :: TVar (Callbacks m)
      , _busQueue          :: TBMQueue Message
      , _workerAsync       :: Async ()
      }

--------------------------------------------------------------------------------
busStop :: MonadIO m => Bus m -> m ()
busStop Bus{..} = atomically $ closeTBMQueue _busQueue

--------------------------------------------------------------------------------
busProcessedEverything :: MonadIO m => Bus m -> m ()
busProcessedEverything Bus{..} = waitAsync _workerAsync

--------------------------------------------------------------------------------
newBus :: MonadIO m => LoggerRef -> m (Bus m)
newBus ref =
  mfix $ \b -> do
    Bus ref <$> (liftIO $ newTVarIO mempty)
            <*> (liftIO $ newTBMQueueIO 500)
            <*> async (worker b)

--------------------------------------------------------------------------------
worker :: MonadIO m => Bus m -> m ()
worker self@Bus{..} = loop
  where
    handleMsg (Message a) = do
      callbacks <- atomically $ readTVar _busEventHandlers
      publishing self callbacks a
      loop

    loop = traverse_ handleMsg =<< atomically (readTBMQueue _busQueue)

--------------------------------------------------------------------------------
instance PubSub m (Bus m) where
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

  publishSTM Bus{..} =
    do closed <- isClosedTBMQueue _busQueue
       writeTBMQueue _busQueue (toMsg a)
       return $ not closed

--------------------------------------------------------------------------------
publishing :: (MonadLogger m, MonadIO m, MonadCatch m, Typeable a)
           => Bus m
           -> Callbacks m
           -> a
           -> m ()
publishing self@Bus{..} callbacks a = do
  let tpe = getType (FromTypeable a)
  logDebug [i|Publishing message #{tpe}.|]
  traverse_ (propagate a) (lookup tpe callbacks)
  logDebug [i|Message #{tpe} propagated.|]

  unless (tpe == messageType) $
    traverse_ (propagate (toMsg a)) (lookup messageType callbacks)

--------------------------------------------------------------------------------
propagate :: (Typeable a, MonadLogger m, MonadCatch m)
          => a
          -> Seq (Callback m)
          -> m ()
propagate a = traverse_ $ \(Callback _ k) -> do
  let Just b = cast a
      tpe    = typeOf b
  outcome <- tryAny $ k b
  case outcome of
    Right _ -> return ()
    Left e  -> logError [i|Exception when propagating #{tpe}: #{e}.|]
