{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RecordWildCards           #-}
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
  ( newStorage ) where

--------------------------------------------------------------------------------
import Data.List.NonEmpty hiding (reverse, length, dropWhile, toList)
import Data.Monoid

--------------------------------------------------------------------------------
import           ClassyPrelude
import qualified Data.HashMap.Strict as H
import           Data.Sequence (Seq, (|>), ViewL(..), viewl)
import           Protocol.Operation
import           Protocol.Types
import           Safe

--------------------------------------------------------------------------------
import Server.FileStorage
import Server.Messages
import Server.Messaging
import Server.RequestId
import Server.Settings
import Server.TransactionLog
import Server.Types

--------------------------------------------------------------------------------
data Stream =
  Stream { streamNextNumber :: EventNumber }

--------------------------------------------------------------------------------
emptyStream :: Stream
emptyStream = Stream 0 mempty

--------------------------------------------------------------------------------
type Streams   = HashMap StreamName Stream
type Pending   = HashMap Guid PendingWrite
type Committed = HashMap EventId CommittedEvent
type Versions  = HashMap StreamName EventNumber

--------------------------------------------------------------------------------
isValidIdempotency :: Committed -> EventNumber -> [EventId] -> IdempotencyResult
isValidIdempotency committed num = notCommitted True
  where
    notCommitted _ [] = Valid num
    notCommitted isFirst (eid:es)
      | member eid committed =
        if isFirst
        then allCommitted es
        else IdempotentError
      | otherwise = notCommitted False es

    allCommitted [] = AlreadyDone num
    allCommitted (eid:es)
      | member eid committed = allCommitted es
      | otherwise            = IdempotentError

--------------------------------------------------------------------------------
data PendingWrite =
  PendingWrite { pendingStream :: StreamName
               , pendingExpVer :: ExpectedVersion
               }

--------------------------------------------------------------------------------
data CommitKey = CommitKey StreamName EventId

--------------------------------------------------------------------------------
data CommittedEvent =
  CommittedEvent { committedPos    :: Integer
                 , committedNumber :: EventNumber
                 }

--------------------------------------------------------------------------------
data Storage =
  Storage { _setts     :: Settings
          , _pub       :: SomePublisher
          , _pendings  :: IORef Pending
          , _committed :: IORef Committed
          , _versions  :: IORef Versions
          , _storage   :: FileStorage
          }

--------------------------------------------------------------------------------
data Msg
  = DoAppend (RequestId WriteEventsResp) StreamName
                                         ExpectedVersion
                                         (NonEmpty Event)
  | DoRead (RequestId ReadEventsResp) StreamName Batch

--------------------------------------------------------------------------------
newStorage :: (Subscribe provider, Publish publisher)
           => Settings
           -> provider
           -> publisher
           -> IO ()
newStorage setts sub pub = do
  s <- Storage setts (asPublisher pub) <$> newIORef mempty
                                       <*> newIORef mempty
                                       <*> newIORef mempty
                                       <*> newFileStorage (dbFile setts)

  subscribe_ sub (onSystemInit s)
  subscribe_ sub (onWritePrepares s)

--------------------------------------------------------------------------------
onSystemInit :: Storage -> SystemInit -> IO ()
onSystemInit Storage{..} _ = publish _pub (Initialized StorageService)

--------------------------------------------------------------------------------
onWritePrepares :: Storage -> WritePrepares -> IO ()
onWritePrepares Storage{..} WritePrepares{..} = do
  vm <- readIORef _versions
  cm <- readIORef _committed

  let eids = eventId <$> preparesEvents
      res  = checkIdempotency preparesVersion preparesName eids vm cm

  case res of
    Valid curNum -> do
      transPos <- currentPosition _storage

      let writing _ []           = return []
          writing isFirst ((e,i):es) = do
            logPos <- currentPosition _storage
            let flags = withFlags $ \f ->
                          case f of
                            TransactionBegin -> isFirst
                            TransactionEnd   -> null es

                prepare = Prepare { prepareTransPos = transPos
                                  , preparePos      = logPos
                                  , prepareFlags    = flags
                                  , prepareExpVer   = curNum + i
                                  , prepareStream   = preparesName
                                  , prepareEventId  = eventId e
                                  , prepareCorrId   = preparesId
                                  , prepareData     = eventPayload e
                                  , prepareMetadata = eventMetadata e
                                  }

            _ <- writePrepare _storage prepare
            (prepare:) <$> writing False es

      prepares <- writing True (zip preparesEvens [0..])
      for_ (lastMay prepares) $ \p -> do
        let ver = prepareExpVer p
            res = WriteOk (EventNumber ver)

        publish _pub (StorageRespMsg preparesId (WriteResult res))
    AlreadyDone curNum -> do
        let res = WriteOk (EventNumber ver)

        publish _pub (StorageRespMsg preparesId (WriteResult res))
    IdempotentError -> do
        let res = WrongExpectedVersion
        publish _pub (StorageRespMsg preparesId (WriteResult res))

--------------------------------------------------------------------------------
data IdempotencyResult
  = Valid EventNumber
  | AlreadyDone EventNumber
  | IdempotentError

--------------------------------------------------------------------------------
checkIdempotency :: ExpectedVersion
                 -> StreamName
                 -> [EventId]
                 -> Versions
                 -> Committed
                 -> IdempotencyResult
checkIdempotency ver name evts ss cc =
  case lookup name ss of
    Nothing ->
      case ver of
        StreamExists   -> IdempotentError
        ExactVersion{} -> IdempotentError
        _              -> Valid 0
    Just curNum ->
      case ver of
        NoStream -> IdempotentError
        ExactVersion n
          | n > curNum  -> IdempotentError
          | n == curNum -> Valid curNum
          | otherwise   -> isValidIdempotency cc curNum evts
        _ -> isValidIdempotency cc curNum evts