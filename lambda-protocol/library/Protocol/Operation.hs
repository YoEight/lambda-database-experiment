--------------------------------------------------------------------------------
-- |
-- Module : Protocol.Operation
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Protocol.Operation where

--------------------------------------------------------------------------------
import Data.List.NonEmpty

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Protocol.Package
import Protocol.Types

--------------------------------------------------------------------------------
data Operation =
  Operation { operationId   :: PkgId
            , operationType :: OperationType
            }

--------------------------------------------------------------------------------
data OperationType
  = WriteEvents StreamName ExpectedVersion (NonEmpty Event)
  | ReadEvents StreamName Batch

--------------------------------------------------------------------------------
data Response =
  Response { responseId   :: PkgId
           , responseType :: ResponseType
           }

--------------------------------------------------------------------------------
data ResponseType
  = WriteEventsResp EventNumber WriteResultFlag
  | ReadEventsResp StreamName [SavedEvent] ReadResultFlag EventNumber Bool
