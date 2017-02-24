{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
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
data Request a where
  WriteEvents :: StreamName
              -> ExpectedVersion
              -> NonEmpty Event
              -> Request WriteEventsResp

  ReadEvents :: StreamName
             -> Batch
             -> Request ReadEventsResp

--------------------------------------------------------------------------------
data WriteEventsResp =
  WriteEventsResp EventNumber WriteResultFlag

--------------------------------------------------------------------------------
data ReadEventsResp =
  ReadEventsResp StreamName [SavedEvent] ReadResultFlag EventNumber Bool

--------------------------------------------------------------------------------
data Operation a =
  Operation { operationId   :: PkgId
            , operationType :: Request a
            }

--------------------------------------------------------------------------------
data SomeOperation = forall a. Typeable a => SomeOperation (Operation a)

--------------------------------------------------------------------------------
data Response a =
  Response { responseId   :: PkgId
           , responseType :: a
           }
