{-# LANGUAGE StrictData #-}
--------------------------------------------------------------------------------
-- |
-- Module : LDE.Internal.CyclicQueue
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module LDE.Internal.CyclicQueue
  ( CyclicQueue
  , newCQ
  , readCQ
  , writeCQ
  , clearCQ
  , updateCQ
  , isEmptyCQ
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
-- | Used to determine if we hit the end of the queue.
data Slot a = Slot a | End

--------------------------------------------------------------------------------
-- | A 'TQueue' that can be cycled.
newtype CyclicQueue a = CyclicQueue (TQueue (Slot a))

--------------------------------------------------------------------------------
-- | Creates an empty 'CyclicQueue'.
newCQ :: IO (CyclicQueue a)
newCQ = fmap CyclicQueue newTQueueIO

--------------------------------------------------------------------------------
-- | Gets an element from the 'CyclicQueue'.
readCQ :: CyclicQueue a -> STM a
readCQ (CyclicQueue q) = do
  Slot a <- readTQueue q
  return a

--------------------------------------------------------------------------------
-- | Writes an element to the 'CyclicQueue'.
writeCQ :: CyclicQueue a -> a -> STM ()
writeCQ (CyclicQueue q) a = writeTQueue q (Slot a)

--------------------------------------------------------------------------------
-- | Empties a 'CyclicQueue'.
clearCQ :: CyclicQueue a -> STM ()
clearCQ (CyclicQueue q) = writeTQueue q End >> go
  where
    go = do
      s <- readTQueue q
      case s of
        End -> return ()
        _   -> go

--------------------------------------------------------------------------------
-- | Updates a 'CyclicQueue'.
updateCQ :: CyclicQueue a -> (a -> STM (Maybe a)) -> STM ()
updateCQ (CyclicQueue q) k = writeTQueue q End >> go
  where
    go = do
      s <- readTQueue q
      case s of
        End    -> return ()
        Slot a -> do
          r <- k a
          case r of
            Nothing -> go
            Just a' -> writeTQueue q (Slot a') >> go

--------------------------------------------------------------------------------
-- | Indicates if a 'CyclicQueue' is empty.
isEmptyCQ :: CyclicQueue a -> STM Bool
isEmptyCQ (CyclicQueue q) = isEmptyTQueue q
