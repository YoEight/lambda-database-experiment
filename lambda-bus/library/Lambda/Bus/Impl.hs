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
      , _busChildren       :: TVar (HashMap UUID (Bus settings))
      , _busParent         :: TVar (Maybe (Bus settings))
      , _workerAsync       :: Async ()
      , _busId             :: UUID
      }

--------------------------------------------------------------------------------
busStop :: Bus settings -> Lambda settings ()
busStop Bus{..} = atomically $ do
  closeTBMQueue _busQueue
  parent <- readTVar _busParent
  for_ parent $ \p ->
    busDeleteChildSTM p _busId

--------------------------------------------------------------------------------
busParent :: Bus settings -> Bus settings -> Lambda settings ()
busParent Bus{..} parent = atomically $ writeTVar _busParent (Just parent)

--------------------------------------------------------------------------------
busProcessedEverything :: Bus settings -> Lambda settings ()
busProcessedEverything Bus{..} = waitAsync _workerAsync

--------------------------------------------------------------------------------
busNewChild :: Bus s -> Lambda s (Bus s)
busNewChild self = do
  child <- newBus
  busInsertChild self child
  return child

--------------------------------------------------------------------------------
busInsertChild :: Bus settings -> Bus settings -> Lambda settings ()
busInsertChild self child = do
  atomically $ modifyTVar' (_busChildren self) (insertMap (_busId child) child)
  busParent child self

--------------------------------------------------------------------------------
busDeleteChildSTM :: Bus settings -> UUID -> STM ()
busDeleteChildSTM Bus{..} childId = modifyTVar' _busChildren (deleteMap childId)

--------------------------------------------------------------------------------
newBus :: Lambda settings (Bus settings)
newBus =
  mfix $ \self -> do
    configure self configureTimer

    Bus <$> (liftIO $ newTVarIO mempty)
        <*> (liftIO $ newTBMQueueIO mailboxLimit)
        <*> (liftIO $ newTVarIO mempty)
        <*> (liftIO $ newTVarIO Nothing)
        <*> async (worker self)
        <*> freshUUID
  where
    mailboxLimit = 500

--------------------------------------------------------------------------------
configure :: Bus settings -> Configure settings () -> Lambda settings ()
configure self conf = do
  atomically . traverse_ (subscribeSTM self) =<< produceCallbacks app
  traverse_ registering (_appTimers app)
  where app = runConfigure conf

        registering (Timer evt timespan plan) =
          registerTimer self (_busId self) evt timespan plan

--------------------------------------------------------------------------------
worker :: Bus settings -> Lambda settings ()
worker self@Bus{..} = loop
  where
    handleMsg msg = do
      callbacks <- atomically $ readTVar _busEventHandlers
      publishing self callbacks msg
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

  publishSTM Bus{..} msg =
    do closed <- isClosedTBMQueue _busQueue
       writeTBMQueue _busQueue msg
       return $ not closed

--------------------------------------------------------------------------------
publishing :: Bus settings
           -> Callbacks settings
           -> Message
           -> Lambda settings ()
publishing self@Bus{..} callbacks msg@(Message a _ destM) =
  unlessM (atomically routedSTM) $
    do let tpe      = getType (FromTypeable a)
           handlers = lookup tpe callbacks

       logDebug [i|Publishing message #{tpe}.|]
       traverse_ (propagate self a) handlers

       -- If there is no handlers this type of event, we try to dispatch it to
       -- its parent bus, if any.
       unless (isJust handlers) $
         do parentM <- liftIO $ readTVarIO _busParent
            for_ parentM $ \parent ->
              void $ atomically $ publishSTM parent msg

       logDebug [i|Message #{tpe} propagated.|]

       traverse_ (propagate self msg) (lookup messageType callbacks)
  where
    routedSTM =
       do children <- readTVar _busChildren
          let known = destM >>= \dest -> lookup dest children
          case known of
            Just child ->
              let newMsg = msg { messageTarget = Nothing }
               in True <$ publishSTM child newMsg
            Nothing -> return False

--------------------------------------------------------------------------------
propagate :: Typeable a => Bus s -> a -> Seq (Callback s) -> Lambda s ()
propagate self@Bus{..} a = traverse_ $ \(Callback _ k) -> do
  let Just b = cast a
      tpe    = typeOf b
  outcome <- tryAny $ runReact (k b) reactEnv
  case outcome of
    Right _ -> return ()
    Left e  -> logError [i|Exception when propagating #{tpe}: #{e}.|]
  where
    reactEnv =
      ReactEnv { _reactBus    = toSomeBus self
               , _reactSelf   = _busId
               , _reactSender = Nothing
               }
