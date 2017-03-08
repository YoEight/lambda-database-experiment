--------------------------------------------------------------------------------
-- |
-- Module : Server.MultiQueuePublisher
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.MultiQueuePublisher
  ( MultiQueuePublisher
  , newMultiQueuePublisher
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Server.Messaging

--------------------------------------------------------------------------------
data MultiQueuePublisher =
  MultiQueuePublisher { _name  :: Text
                      , _pubs  :: Vector SomePublisher
                      , _queue :: TQueue Message
                      , _ref   :: IORef Int
                      }

--------------------------------------------------------------------------------
newMultiQueuePublisher :: Text -> [SomePublisher] -> IO MultiQueuePublisher
newMultiQueuePublisher name pubs = do
  q <- MultiQueuePublisher name (fromList pubs) <$> newTQueueIO
                                                <*> newIORef 0

  let action = do
        _ <- forkFinally (worker q) $ \outcome ->
          case outcome of
            Left _  -> action
            Right _ -> return ()

        return ()
  action
  return q

--------------------------------------------------------------------------------
getPub :: MultiQueuePublisher -> IO SomePublisher 
getPub MultiQueuePublisher{..} = do
  idx <- atomicModifyIORef' _ref $ \i ->
    (succ i, i `mod` length _pubs)

  return $ indexEx _pubs idx

--------------------------------------------------------------------------------
worker :: MultiQueuePublisher -> IO ()
worker q@MultiQueuePublisher{..} = forever $ do
  msg <- atomically $ readTQueue _queue
  case msg of
    Message a -> do
      pub <- getPub q
      publish pub a

--------------------------------------------------------------------------------
instance Publish MultiQueuePublisher where
  publish MultiQueuePublisher{..} a =
    atomically $ writeTQueue _queue (Message a)