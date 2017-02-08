--------------------------------------------------------------------------------
-- |
-- Module : Server.Messaging
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Messaging where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Functor.Contravariant

--------------------------------------------------------------------------------
newtype Await a = Await { awaitMsgSTM :: STM a }

--------------------------------------------------------------------------------
awaitMsg :: MonadIO m => Await a -> m a
awaitMsg w = atomically $ awaitMsgSTM w

--------------------------------------------------------------------------------
instance Functor Await where
  fmap f (Await m) = Await (fmap f m)

--------------------------------------------------------------------------------
instance Applicative Await where
  pure a = Await $ pure a

  Await f <*> Await a = Await (f <*> a)

--------------------------------------------------------------------------------
instance Alternative Await where
  empty = Await empty

  Await a <|> Await b = Await (a <|> b)

--------------------------------------------------------------------------------
newtype Publish a = Publish { publishSTM :: a -> STM () }

--------------------------------------------------------------------------------
instance Contravariant Publish where
  contramap f (Publish k) = Publish (k . f)

--------------------------------------------------------------------------------
publish :: MonadIO m => Publish a -> a -> m ()
publish p a = atomically $ publishSTM p a

--------------------------------------------------------------------------------
newExchange :: IO (Await a, Publish a)
newExchange = do
  q <- newTQueueIO
  return (Await $ readTQueue q, Publish $ writeTQueue q)
