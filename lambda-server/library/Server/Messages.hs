--------------------------------------------------------------------------------
-- |
-- Module : Server.Messages
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Messages where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.List.NonEmpty
import Protocol.Package
import Protocol.Operation
import Protocol.Types

--------------------------------------------------------------------------------
import Server.Types

--------------------------------------------------------------------------------
data StorageReqMsg
  = StorageReqMsg { storageReqId   :: Guid
                  , storageReqType :: StorageReqType
                  }

--------------------------------------------------------------------------------
data StorageRespMsg
  = StorageRespMsg { storageRespId   :: Guid
                   , storageRespType :: StorageRespType
                   }

--------------------------------------------------------------------------------
data StorageReqType
  = StorageAppendStream { storageReqName   :: StreamName
                        , storageReqVer    :: ExpectedVersion
                        , storageReqEvents :: NonEmpty Event
                        }

  | StorageReadStream { storageReqName  :: StreamName
                      , storageReqBatch :: Batch
                      }

--------------------------------------------------------------------------------
data StorageRespType
  = WriteResult (WriteResult EventNumber)
  | ReadResult  StreamName (ReadResult [SavedEvent])

--------------------------------------------------------------------------------
data TransactionLogMsg
  = PreparedWrites { pwritesId     :: TransactionId
                   , pwritesEvents :: Seq Entry
                   , pwritesNext   :: Int
                   }

--------------------------------------------------------------------------------
data TcpSend = TcpSend ConnectionId Pkg

--------------------------------------------------------------------------------
data RecvPkg = RecvPkg Pkg

--------------------------------------------------------------------------------
data Heartbeat = Heartbeat Integer

--------------------------------------------------------------------------------
data HeartbeatTimeout = HeartbeatTimeout Integer

--------------------------------------------------------------------------------
data Shutdown = Shutdown

--------------------------------------------------------------------------------
data SystemInit = SystemInit

--------------------------------------------------------------------------------
data ServiceName
  = TransactionLog
  deriving (Enum, Show)

--------------------------------------------------------------------------------
data Initialized = Initialized ServiceName

--------------------------------------------------------------------------------
data SystemInitFailure = SystemInitFailure ServiceName SomeException

--------------------------------------------------------------------------------
data NewOperation = NewOperation ConnectionId SomeOperation

--------------------------------------------------------------------------------
data WritePrepares =
  WritePrepares { preparesEvents  :: [Event]
                , preparesVersion :: ExpectedVersion
                , preparesId      :: Guid
                , preparesName    :: StreamName
                }

--------------------------------------------------------------------------------
data WritePrepared =
  WritePrepared { preparedId     :: Guid
                , preparedEvents :: [Prepared]
                }