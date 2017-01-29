{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : LDE.Client
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module LDE.Client
  ( Client
  , WriteResult(..)
  , WriteFailure(..)
  , ReadResult(..)
  , ReadFailure(..)
  , newClient
  , writeEvents
  , readEvents
  ) where

--------------------------------------------------------------------------------
import Data.List.NonEmpty

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Foldable
import Protocol.Operation
import Protocol.Types

--------------------------------------------------------------------------------
import LDE.Internal.Command
import LDE.Internal.Exec
import LDE.Internal.Settings

--------------------------------------------------------------------------------
data Client = Client { _exec :: Exec }

--------------------------------------------------------------------------------
newClient :: ConnectionType -> IO Client
newClient tpe = Client <$> newExec (defaultSettings tpe)

--------------------------------------------------------------------------------
data WriteResult
  = WriteOk EventNumber
  | WriteFailure WriteFailure

--------------------------------------------------------------------------------
data WriteFailure = WrongExpectedVersion deriving Show

--------------------------------------------------------------------------------
instance Exception WriteFailure

--------------------------------------------------------------------------------
writeEvents :: Client
            -> StreamName
            -> ExpectedVersion
            -> NonEmpty Event
            -> IO (Async WriteResult)
writeEvents Client{..} n ver evts = do
  (a, k) <- newPromise

  let cmd = Command { commandReq = WriteEvents n ver evts
                    , commandCb  = \(WriteEventsResp num flag) ->
                        case flag of
                          WriteSuccess ->
                            k (WriteOk num)
                          WriteWrongExpectedVersion ->
                            k (WriteFailure WrongExpectedVersion)
                    }

  execSubmit _exec cmd
  return a

--------------------------------------------------------------------------------
data ReadResult a
  = ReadOk a
  | ReadFailure ReadFailure
  deriving Show

--------------------------------------------------------------------------------
data ReadFailure = NoStreamFound deriving Show

--------------------------------------------------------------------------------
instance Functor ReadResult where
  fmap f (ReadOk a)      = ReadOk $ f a
  fmap _ (ReadFailure e) = ReadFailure e

--------------------------------------------------------------------------------
instance Foldable ReadResult where
  foldMap f (ReadOk a) = f a
  foldMap _ _          = mempty

--------------------------------------------------------------------------------
-- | Represents batch of events read from a store.
data Slice =
  Slice { sliceEvents          :: [SavedEvent]
        , sliceEndOfStream     :: Bool
        , sliceNextEventNumber :: EventNumber
        } deriving Show

--------------------------------------------------------------------------------
readEvents :: Client -> StreamName -> Batch -> IO (Async (ReadResult Slice))
readEvents Client{..} name b = do
  (a, k) <- newPromise

  let cmd = Command { commandReq = ReadEvents name b
                    , commandCb  = \(ReadEventsResp _ evts flag num eos) ->
                        case flag of
                          ReadSuccess ->
                            let s = Slice evts eos num in k (ReadOk s)
                          ReadNoStream ->
                            let e = ReadFailure NoStreamFound in k e
                    }

  execSubmit _exec cmd
  return a

--------------------------------------------------------------------------------
newPromise :: IO (Async a, a -> IO ())
newPromise = do
  mvar <- newEmptyMVar
  a <- async $ readMVar mvar
  return (a, putMVar mvar)
