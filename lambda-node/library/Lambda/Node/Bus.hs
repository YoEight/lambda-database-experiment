--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Node.Bus
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Bus where

--------------------------------------------------------------------------------
import Data.Typeable

--------------------------------------------------------------------------------
import Lambda.Node.Logger
import Lambda.Node.Monitoring
import Lambda.Node.Prelude
import Lambda.Node.Settings
import Lambda.Node.Types

--------------------------------------------------------------------------------
type Callbacks = HashMap Type (Seq Callback)

--------------------------------------------------------------------------------
data Bus =
  Bus { _name          :: Text
      , _runtime       :: Runtime
      , _eventHandlers :: TVar Callbacks
      , _worker        :: Async ()
      , _queue         :: TBMQueue Message
      }

--------------------------------------------------------------------------------
newBus :: Runtime -> Text -> IO Bus
newBus runtime name = do
  mfix $ \self ->
    Bus name runtime <$> newTVarIO mempty
                     <*> async (processMessages self)
                     <*> newTBMQueueIO 500

--------------------------------------------------------------------------------
instance PubSub Bus where
  subscribeSTM Bus{..} cb@(Callback prx _) =
    modifyTVar' _eventHandlers update
    where
      update callbacks =
        let tpe  = getType (FromProxy prx)
            next = alterMap $ \input ->
              case input of
                Nothing -> Just (singleton cb)
                Just hs -> Just (snoc hs cb) in
        next tpe callbacks

  publishSTM Bus{..} a = do
    closed <- isClosedTBMQueue _queue
    writeTBMQueue _queue (toMsg a)
    return $ not closed

--------------------------------------------------------------------------------
processMessages :: Bus -> IO ()
processMessages self@Bus{..} = loop
  where
    handleMsg (Message a) = do
      callbacks <- readTVarIO _eventHandlers
      publishing self callbacks a
      loop

    loop = traverse_ handleMsg =<< atomically (readTBMQueue _queue)

--------------------------------------------------------------------------------
publishing :: Typeable a => Bus -> Callbacks -> a -> IO ()
publishing self@Bus{..} callbacks a = do
  let tpe = getType (FromTypeable a)
  runServer _runtime self $ do
    logDebug [i|Publishing message #{tpe}.|]
    traverse_ (propagate a) (lookup tpe callbacks)
    logDebug [i|Message #{tpe} propagated.|]

    unless (tpe == messageType) $
      traverse_ (propagate (toMsg a)) (lookup messageType callbacks)

--------------------------------------------------------------------------------
propagate :: Typeable a => a -> Seq Callback -> Server ()
propagate a = traverse_ $ \(Callback _ k) -> do
  let Just b = cast a
      tpe    = typeOf b
  outcome <- tryAny $ k b
  case outcome of
    Right _ -> return ()
    Left e  -> logError [i|Exception when propagating #{tpe}: #{e}.|]