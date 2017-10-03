{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Node.Journal
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Journal
  ( Journal
  , InMemory
  , newInMemory
  , runInMemory
  , setPos
  , getPos
  , marshal
  , unmarshal
  ) where

--------------------------------------------------------------------------------
import qualified Control.Monad.Operational as Operational
import           Data.Serialize
import           Lambda.Prelude

--------------------------------------------------------------------------------
import Lambda.Node.Settings

--------------------------------------------------------------------------------
type Journal o = Operational.Program Op o

--------------------------------------------------------------------------------
data Op a where
  SetPos    :: Integer -> Op ()
  GetPos    :: Op Integer
  Marshal   :: Serialize a => a -> Op Integer
  Unmarshal :: Serialize a => Op a

--------------------------------------------------------------------------------
setPos :: Integer -> Journal ()
setPos pos = Operational.singleton (SetPos pos)

--------------------------------------------------------------------------------
getPos :: Journal Integer
getPos = Operational.singleton GetPos

--------------------------------------------------------------------------------
marshal :: Serialize a => a -> Journal Integer
marshal a = Operational.singleton (Marshal a)

--------------------------------------------------------------------------------
unmarshal :: Serialize a => Journal a
unmarshal = Operational.singleton Unmarshal

--------------------------------------------------------------------------------
data InMemory =
  InMemory { _memCurPos :: IORef Integer
           , _memLogs   :: IORef (HashMap Integer ByteString)
           }

--------------------------------------------------------------------------------
newInMemory :: Lambda Settings InMemory
newInMemory =
  InMemory <$> newIORef 0
           <*> newIORef mempty

--------------------------------------------------------------------------------
runInMemory :: forall m a. MonadBase IO m
            => InMemory
            -> Journal a
            -> m a
runInMemory self sm = Operational.interpretWithMonad go sm
  where
    go :: forall i. Op i -> m i
    go (SetPos pos) =
      writeIORef (_memCurPos self) pos

    go GetPos =
      readIORef (_memCurPos self)

    go (Marshal a) = do
      pos <- readIORef (_memCurPos self)
      let bytes = encode a
      modifyIORef' (_memLogs self) (insertMap pos bytes)
      writeIORef (_memCurPos self) (pos + (fromIntegral $ length bytes))
      pure pos

    go Unmarshal = do
      pos <- readIORef (_memCurPos self)
      m   <- readIORef (_memLogs self)
      case lookup pos m of
        Nothing    -> liftBase $ fail "End of journal reached"
        Just bytes ->
          case decode bytes of
            Right a -> pure a
            _       -> liftBase $ fail "Failed to unmarshal"
