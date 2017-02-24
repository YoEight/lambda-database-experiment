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
newtype Await a = Await { awaitMsg :: IO a }

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
newtype Publish a = Publish { publish :: a -> IO () }

--------------------------------------------------------------------------------
instance Contravariant Publish where
  contramap f (Publish k) = Publish (k . f)

--------------------------------------------------------------------------------
newExchange :: IO (Await a, Publish a)
newExchange = do
  q <- newTQueueIO
  let reading = atomically $ readTQueue q
      writing = atomically . writeTQueue q
  return (Await reading, Publish writing)
