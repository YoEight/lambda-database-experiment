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
-- In-memory message bus implementation. This implementation also provides
-- tree-like organisation. A bus could have children and also one parent.
--
-- When a bus receive a message but has no registered callbacks for it, it
-- propagates that message to its parent. That procedure stops as soon as
-- a callbacks can support the message or there is parent to reach out.
--------------------------------------------------------------------------------
module Lambda.Bus.Impl where

--------------------------------------------------------------------------------
import Data.Typeable

--------------------------------------------------------------------------------
import Lambda.Logger
import Lambda.Prelude

--------------------------------------------------------------------------------
import Lambda.Bus.Types

--------------------------------------------------------------------------------
type Callbacks settings = HashMap Type (Seq (Callback settings))

--------------------------------------------------------------------------------
-- | An in-memory message bus.
data Bus settings =
  Bus { _busEventHandlers :: TVar (Callbacks settings)
        -- ^ All registered callbacks.
      , _busQueue :: TBMQueue Message
        -- ^ Actual message queue.
      , _busChildren :: TVar (HashMap UUID (Bus settings))
        -- ^ Bus children.
      , _busParent :: TVar (Maybe (Bus settings))
        -- ^ Bus parent.
      , _workerAsync :: Async ()
        -- ^ Worker thread handle.
      , _busId :: UUID
        -- ^ Bus id.
      }

--------------------------------------------------------------------------------
-- | Stops 'Bus'.
_busStop :: Bus settings -> Lambda settings ()
_busStop Bus{..} = atomically $ do
  closeTBMQueue _busQueue
  parent <- readTVar _busParent
  for_ parent $ \p ->
    busDeleteChildSTM p _busId

--------------------------------------------------------------------------------
-- | Set a 'Bus' parent.
busParent :: Bus settings -> Bus settings -> Lambda settings ()
busParent Bus{..} parent = atomically $ writeTVar _busParent (Just parent)

--------------------------------------------------------------------------------
-- | Waits until a 'Bus' has stopped and carried out all its messages.
busProcessedEverything :: MonadIO m => Bus settings -> m ()
busProcessedEverything Bus{..} = waitAsync _workerAsync

--------------------------------------------------------------------------------
-- | Creates a new child bus.
busNewChild :: Bus s -> Lambda s (Bus s)
busNewChild self = do
  child <- newBus
  busInsertChild self child
  return child

--------------------------------------------------------------------------------
-- | Insert a bus as a child.
busInsertChild :: Bus settings -- Parent.
               -> Bus settings -- Child.
               -> Lambda settings ()
busInsertChild self child = do
  atomically $ modifyTVar' (_busChildren self) (insertMap (_busId child) child)
  busParent child self

--------------------------------------------------------------------------------
-- | Remove a bus as child given its id.
busDeleteChildSTM :: Bus settings -> UUID -> STM ()
busDeleteChildSTM Bus{..} childId = modifyTVar' _busChildren (deleteMap childId)

--------------------------------------------------------------------------------
-- | Creates a new 'Bus'.
newBus :: Lambda settings (Bus settings)
newBus = do
  bus <- mfix $ \self ->
    Bus <$> (liftIO $ newTVarIO mempty)
        <*> (liftIO $ newTBMQueueIO mailboxLimit)
        <*> (liftIO $ newTVarIO mempty)
        <*> (liftIO $ newTVarIO Nothing)
        <*> async (worker self)
        <*> freshUUID

  configure bus configureTimer
  return bus
  where
    mailboxLimit = 500

--------------------------------------------------------------------------------
-- | Configures a 'Bus' with the help of a 'Configure' computation.
configure :: Bus settings -> Configure settings () -> Lambda settings ()
configure self conf = runConfigure conf self

--------------------------------------------------------------------------------
-- | 'Bus' worker thread.
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

  busId = _busId
  busStop = _busStop

--------------------------------------------------------------------------------
-- | Publishes a message. First it determines if the message is routed. Meaning
--   if the message has proper target known by its children. If yes, then the
--   message is dispatched to the right child. Otherwise, it tries to dispatch
--   the message to the right handler if it exists. This implementation also
--   propagating the message to handler that support 'Message' if those exists.
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
-- | Runs every callbacks in order by submitting the message to them.
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
