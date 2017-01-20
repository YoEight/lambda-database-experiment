--------------------------------------------------------------------------------
-- |
-- Module : Server.Storage
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Storage
  ( Storage
  , WriteResult(..)
  , ReadResult(..)
  , WriteFailure(..)
  , ReadFailure(..)
  , newInMemoryStorage
  , appendStream
  , readStream
  ) where

--------------------------------------------------------------------------------
import Data.List.NonEmpty

--------------------------------------------------------------------------------
import ClassyPrelude
import Protocol.Types

--------------------------------------------------------------------------------
import Server.Settings

--------------------------------------------------------------------------------
data Storage =
  Storage

--------------------------------------------------------------------------------
data WriteResult a
  = WriteOk a
  | WriteFailed WriteFailure

--------------------------------------------------------------------------------
data WriteFailure
  = WrongExpectedVersion

--------------------------------------------------------------------------------
newInMemoryStorage :: Settings -> IO Storage
newInMemoryStorage _ = return Storage

--------------------------------------------------------------------------------
appendStream :: Storage
             -> StreamName
             -> ExpectedVersion
             -> NonEmpty Event
             -> IO (WriteResult EventNumber)
appendStream = undefined

--------------------------------------------------------------------------------
data ReadResult a
  = ReadOk EventNumber Bool a
  | ReadFailed ReadFailure

--------------------------------------------------------------------------------
data ReadFailure
  = StreamNotFound

--------------------------------------------------------------------------------
readStream :: Storage -> StreamName -> Batch -> IO (ReadResult [SavedEvent])
readStream = undefined
