--------------------------------------------------------------------------------
-- |
-- Module : Server.Types
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Types where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Serialize

--------------------------------------------------------------------------------
import Protocol.Types

--------------------------------------------------------------------------------
newtype TransactionId = TransactionId Guid
  deriving ( Eq
           , Ord
           , Show
           , Serialize
           , Hashable
           )

--------------------------------------------------------------------------------
instance FreshId TransactionId where
  freshId = TransactionId <$> freshId

--------------------------------------------------------------------------------
data WriteResult a
  = WriteOk a
  | WriteFailed WriteFailure

--------------------------------------------------------------------------------
data WriteFailure
  = WrongExpectedVersion

--------------------------------------------------------------------------------
data ReadResult a
  = ReadOk EventNumber Bool a
  | ReadFailed ReadFailure

--------------------------------------------------------------------------------
data ReadFailure
  = StreamNotFound

--------------------------------------------------------------------------------
type SeekPos = Integer

--------------------------------------------------------------------------------
data Entry =
  Entry { entryEventId :: EventId
        , entrySeekPos :: SeekPos
        }

--------------------------------------------------------------------------------
newtype ConnectionId = ConnectionId Guid
  deriving ( Eq
           , Ord
           , Serialize
           , Hashable
           )

--------------------------------------------------------------------------------
instance FreshId ConnectionId where
  freshId = ConnectionId <$> freshId

--------------------------------------------------------------------------------
instance Show ConnectionId where
  show (ConnectionId g) = "connection-" <> show g