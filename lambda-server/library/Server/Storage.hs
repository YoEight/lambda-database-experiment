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
  ( Storage
  , WriteResult(..)
  , ReadResult(..)
  , WriteFailure(..)
  , ReadFailure(..)
  , SomeStorageMsg(..)
  , StorageMsg(..)
  , newInMemoryStorage
  , appendStream
  , readStream
  ) where

--------------------------------------------------------------------------------
import Data.List.NonEmpty hiding (reverse, length, dropWhile)
import Data.Monoid

--------------------------------------------------------------------------------
import           ClassyPrelude
import qualified Data.HashMap.Strict as H
import           Data.Sequence (Seq, (|>), ViewL(..), viewl)
import           Protocol.Operation
import           Protocol.Types

--------------------------------------------------------------------------------
import Server.Messaging
import Server.RequestId
import Server.Settings

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
          , _pub       :: Publish SomeStorageMsg
          , _chan      :: Chan Msg
          , streamsVar :: TVar Streams
          }

--------------------------------------------------------------------------------
data WriteResult a
  = WriteOk a
  | WriteFailed WriteFailure

--------------------------------------------------------------------------------
data WriteFailure
  = WrongExpectedVersion

--------------------------------------------------------------------------------
data Msg
  = DoAppend (RequestId WriteEventsResp) StreamName
                                         ExpectedVersion
                                         (NonEmpty Event)
  | DoRead (RequestId ReadEventsResp) StreamName Batch

--------------------------------------------------------------------------------
data SomeStorageMsg =
  forall a. Typeable a => SomeStorageMsg (RequestId a) (StorageMsg a)

--------------------------------------------------------------------------------
data StorageMsg a where
  WriteResult :: WriteResult EventNumber -> StorageMsg WriteEventsResp
  ReadResult  :: StreamName
              -> ReadResult [SavedEvent]
              -> StorageMsg ReadEventsResp

--------------------------------------------------------------------------------
newInMemoryStorage :: Settings -> Publish SomeStorageMsg -> IO Storage
newInMemoryStorage setts pub = do

   s <- Storage setts pub <$> newChan
                          <*> newTVarIO mempty

   let action = do
         _ <- forkFinally (worker s) $ \_ -> action
         return ()

   action
   return s

--------------------------------------------------------------------------------
appendStream :: Storage
             -> StreamName
             -> ExpectedVersion
             -> NonEmpty Event
             -> IO (RequestId WriteEventsResp)
appendStream Storage{..} n e es = do
  rid <- freshRequestId
  writeChan _chan (DoAppend rid n e es)
  return rid

--------------------------------------------------------------------------------
readStream :: Storage
           -> StreamName
           -> Batch
           -> IO (RequestId ReadEventsResp)
readStream Storage{..} s b = do
  rid <- freshRequestId
  writeChan _chan (DoRead rid s b)
  return rid

--------------------------------------------------------------------------------
worker :: Storage -> IO ()
worker s@Storage{..} = forever (readChan _chan >>= go)
  where
    go (DoAppend rid n e es) = do
      r <- doAppendStream s n e es
      publish _pub (SomeStorageMsg rid $ WriteResult r)
    go (DoRead rid n b) = do
      r <- doReadStream s n b
      publish _pub (SomeStorageMsg rid $ ReadResult n r)

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
data ReadResult a
  = ReadOk EventNumber Bool a
  | ReadFailed ReadFailure

--------------------------------------------------------------------------------
data ReadFailure
  = StreamNotFound

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
