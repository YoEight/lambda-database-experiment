--------------------------------------------------------------------------------
-- |
-- Module : Server.QueuePublisher
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.QueuePublisher
  ( QueuePublisher
  , newQueuePublisher
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Server.Bus
import Server.Messaging

--------------------------------------------------------------------------------
data QueuePublisher =
  QueuePublisher { _name  :: Text
                 , _bus   :: Bus
                 , _queue :: TQueue Message }

--------------------------------------------------------------------------------
newQueuePublisher :: Text -> Bus -> IO QueuePublisher
newQueuePublisher name bus = do
  q <- QueuePublisher name bus <$> newTQueueIO

  let action = do
        _ <- forkFinally (worker q) $ \res ->
          case res of
            Right _ -> return ()
            Left _  -> action
        return ()

  action
  return q

--------------------------------------------------------------------------------
worker :: QueuePublisher -> IO ()
worker q = loop
  where
    loop = do
      msg <- atomically $ readTQueue $ _queue q
      case msg of
        Message a -> do
          _ <- tryAny $ publish (_bus q) a
          loop

--------------------------------------------------------------------------------
instance Publish QueuePublisher where
  publish q a = atomically $ writeTQueue (_queue q) (Message a)
