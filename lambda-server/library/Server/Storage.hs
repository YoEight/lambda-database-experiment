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
  ( newInMemoryStorage ) where

--------------------------------------------------------------------------------
import Data.List.NonEmpty hiding (reverse, length, dropWhile, toList)
import Data.Monoid

--------------------------------------------------------------------------------
import           ClassyPrelude
import qualified Data.HashMap.Strict as H
import           Data.Sequence (Seq, (|>), ViewL(..), viewl)
import           Protocol.Operation
import           Protocol.Types

--------------------------------------------------------------------------------
import Server.Messages
import Server.Messaging
import Server.RequestId
import Server.Settings
import Server.Types

--------------------------------------------------------------------------------
data Stream =
  Stream { streamNextNumber :: EventNumber
         , streamEvents     :: Seq SavedEvent
         }

--------------------------------------------------------------------------------
emptyStream :: Stream
emptyStream = Stream 0 mempty

--------------------------------------------------------------------------------
type Streams = HashMap StreamName Stream

--------------------------------------------------------------------------------
data Storage =
  Storage { _setts     :: Settings
          , _pub       :: SomePublisher
          , streamsVar :: TVar Streams
          }

--------------------------------------------------------------------------------
data Msg
  = DoAppend (RequestId WriteEventsResp) StreamName
                                         ExpectedVersion
                                         (NonEmpty Event)
  | DoRead (RequestId ReadEventsResp) StreamName Batch

--------------------------------------------------------------------------------
newInMemoryStorage :: (Subscribe provider, Publish publisher)
                   => Settings
                   -> provider
                   -> publisher
                   -> IO ()
newInMemoryStorage setts sub pub = do
  s <- Storage setts (asPublisher pub) <$> newTVarIO mempty

  subscribe_ sub (onStorageRequest s)
  subscribe_ sub (onTransactionLogMsg s)

--------------------------------------------------------------------------------
onStorageRequest :: Storage -> StorageReqMsg -> IO ()
onStorageRequest s (StorageReqMsg rid tpe) =
  case tpe of
    StorageAppendStream n ver xs ->
      onAppendStream s rid n ver xs
    StorageReadStream n b ->
      onReadStream s rid n b

--------------------------------------------------------------------------------
onAppendStream :: Storage
               -> Guid
               -> StreamName
               -> ExpectedVersion
               -> NonEmpty Event
               -> IO ()
onAppendStream Storage{..} gid n ver xs =
  publish _pub msg
  where
    msg = WritePrepares { preparesEvents  = toList xs
                        , preparesVersion = ver
                        , preparesId      = gid
                        , preparesName    = n
                        }

--------------------------------------------------------------------------------
onReadStream :: Storage
             -> Guid
             -> StreamName
             -> Batch
             -> IO ()
onReadStream _ _ n b = return ()

--------------------------------------------------------------------------------
onTransactionLogMsg :: Storage -> TransactionLogMsg -> IO ()
onTransactionLogMsg s msg =
  case msg of
    PreparedWrites tid events next ->
      onPreparedWrites s tid events next

--------------------------------------------------------------------------------
onPreparedWrites :: Storage
                 -> TransactionId
                 -> Seq Entry
                 -> Int
                 -> IO ()
onPreparedWrites _ tid events next = return ()

-- --------------------------------------------------------------------------------
-- appendStream :: Storage
--              -> StreamName
--              -> ExpectedVersion
--              -> NonEmpty Event
--              -> IO (RequestId WriteEventsResp)
-- appendStream Storage{..} n e es = do
--   rid <- freshRequestId
--   writeChan _chan (DoAppend rid n e es)
--   return rid

-- --------------------------------------------------------------------------------
-- readStream :: Storage
--            -> StreamName
--            -> Batch
--            -> IO (RequestId ReadEventsResp)
-- readStream Storage{..} s b = do
--   rid <- freshRequestId
--   writeChan _chan (DoRead rid s b)
--   return rid

-- --------------------------------------------------------------------------------
-- worker :: Storage -> IO ()
-- worker s@Storage{..} = forever (readChan _chan >>= go)
--   where
--     go (DoAppend rid n e es) = do
--       r <- doAppendStream s n e es
--       publish _pub (SomeStorageMsg rid $ WriteResult r)
--     go (DoRead rid n b) = do
--       r <- doReadStream s n b
--       publish _pub (SomeStorageMsg rid $ ReadResult n r)

--------------------------------------------------------------------------------
doAppendStream :: Storage
               -> StreamName
               -> ExpectedVersion
               -> NonEmpty Event
               -> IO (WriteResult EventNumber)
doAppendStream Storage{..} name ver xs = atomically $ do
  streams <- readTVar streamsVar

  case canSaveEvents ver name xs streams of
    CanSave stream  -> WriteOk <$> alterStream streams stream
    AlreadyDone num -> return $ WriteOk num
    Nope            -> return $ WriteFailed WrongExpectedVersion
  where
    alterStream streams stream =
      let (nxtNum, newStream) = _appendStream xs stream
          streams'            = insertMap name newStream streams in

      nxtNum <$ writeTVar streamsVar streams'

--------------------------------------------------------------------------------
streamEventsAfterNum :: EventNumber -> Stream -> Seq SavedEvent
streamEventsAfterNum n = dropWhile ((< n) . eventNumber) . streamEvents

--------------------------------------------------------------------------------
data SaveOutcome
  = CanSave Stream
  | AlreadyDone EventNumber
  | Nope

--------------------------------------------------------------------------------
canSaveEvents :: ExpectedVersion
              -> StreamName
              -> NonEmpty Event
              -> Streams
              -> SaveOutcome
canSaveEvents ver name evts ss =
  case lookup name ss of
    Nothing ->
      case ver of
        StreamExists   -> Nope
        ExactVersion{} -> Nope
        _              -> CanSave emptyStream
    Just stream -> let curNum = streamNextNumber stream in
      case ver of
        StreamExists -> CanSave stream
        NoStream     -> Nope
        AnyVersion   -> checkConcurrentConflict Nothing stream evts
        ExactVersion n
          | n > curNum  -> Nope
          | n == curNum -> CanSave stream
          | otherwise   -> checkConcurrentConflict (Just n) stream evts

--------------------------------------------------------------------------------
checkConcurrentConflict :: Maybe EventNumber
                        -> Stream
                        -> NonEmpty Event
                        -> SaveOutcome
checkConcurrentConflict usecase stream evts =
  case usecase of
    Just{} -> if null committedMap
                then AlreadyDone (streamNextNumber stream)
                else Nope

    Nothing -> if null committedMap
               then AlreadyDone (streamNextNumber stream)
               -- Bad implementation, checking those maps have the same length
               -- isn't enough. We need to make sure those contain the same
               -- event ids too.
               else if length initMap == length committedMap
                    then CanSave stream
                    else Nope
  where
    initMap = flip foldMap evts $ \e ->
      H.singleton (eventId e) ()

    streamTail =
      case usecase of
        Just num -> streamEventsAfterNum num stream
        Nothing  -> streamEvents stream

    -- FIXME - Stop the recursion as soon the map is empty.
    allCommitted = flip foldl' initMap $ \m s ->
      let eid = eventId (savedEvent s) in deleteMap eid m

    committedMap = allCommitted streamTail

--------------------------------------------------------------------------------
doReadStream :: Storage -> StreamName -> Batch -> IO (ReadResult [SavedEvent])
doReadStream Storage{..} name b = atomically $ do
  streams <- readTVar streamsVar

  case lookup name streams of
    Nothing     -> return $ ReadFailed StreamNotFound
    Just stream -> do
      let (nxt, eos, evts) = _readStream b stream

      return $ ReadOk nxt eos evts

--------------------------------------------------------------------------------
_appendStream :: NonEmpty Event -> Stream -> (EventNumber, Stream)
_appendStream xs ss = foldl' go ((-1), ss) xs
  where
    go (_, s) e =
      let num    = streamNextNumber s
          evts   = streamEvents s
          nxtNum = num + 1
          s'     = s { streamNextNumber = nxtNum
                     , streamEvents     = evts |> SavedEvent num e
                     } in
      (nxtNum, s')

--------------------------------------------------------------------------------
_readStream :: Batch -> Stream -> (EventNumber, Bool, [SavedEvent])
_readStream Batch{..} Stream{..}
  | batchFrom >= streamNextNumber = ((-1), True, [])
  | batchFrom < 0 = (streamNextNumber, False, [])
  | otherwise = go 0 (-1) [] streamEvents
  where
    go i nxt xs cur =
      case viewl cur of
        EmptyL -> (nxt, True, reverse xs)
        x :< rest
          | eventNumber x < batchFrom -> go i (eventNumber x) xs rest
          | i <= batchSize -> go (i+1) (eventNumber x) (x:xs) rest
          | otherwise -> (nxt, False, reverse xs)
