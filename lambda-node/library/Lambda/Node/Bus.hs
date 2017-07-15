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
processMessages = undefined