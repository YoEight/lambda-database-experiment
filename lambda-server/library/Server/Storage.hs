{-# LANGUAGE RecordWildCards #-}
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
import Data.Monoid

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Sequence (Seq, (|>))
import Protocol.Types

--------------------------------------------------------------------------------
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
type Streams = Map StreamName Stream

--------------------------------------------------------------------------------
data Storage =
  Storage { streamsVar :: TVar Streams }

--------------------------------------------------------------------------------
data WriteResult a
  = WriteOk a
  | WriteFailed WriteFailure

--------------------------------------------------------------------------------
data WriteFailure
  = WrongExpectedVersion

--------------------------------------------------------------------------------
newInMemoryStorage :: Settings -> IO Storage
newInMemoryStorage _ =
  Storage <$> newTVarIO mempty

--------------------------------------------------------------------------------
assumeExistence :: ExpectedVersion -> Maybe (Maybe EventNumber)
assumeExistence StreamExists     = Just Nothing
assumeExistence (ExactVersion n) = Just $ Just n
assumeExistence AnyVersion       = Just Nothing
assumeExistence NoStream         = Nothing

--------------------------------------------------------------------------------
assumeNonExistence :: ExpectedVersion -> Bool
assumeNonExistence AnyVersion = True
assumeNonExistence NoStream   = True
assumeNonExistence _          = False

--------------------------------------------------------------------------------
appendStream :: Storage
             -> StreamName
             -> ExpectedVersion
             -> NonEmpty Event
             -> IO (WriteResult EventNumber)
appendStream Storage{..} name ver xs = atomically $ do
  streams <- readTVar streamsVar

  let alterStream stream =
        let (nxtNum, newStream) = _appendStream xs stream
            streams'            = insertMap name newStream streams in

        nxtNum <$ writeTVar streamsVar streams'

  case lookup name streams of
    Nothing ->
      if assumeNonExistence ver
      then
        WriteOk <$> alterStream emptyStream
      else
        return $ WriteFailed WrongExpectedVersion

    Just stream ->
      case assumeExistence ver of
        Nothing     -> return $ WriteFailed WrongExpectedVersion
        Just exactM -> do
          let curNum      = streamNextNumber stream
              matchExpVer = getAll $ foldMap (All . (== curNum)) exactM

          if matchExpVer
          then
            WriteOk <$> alterStream stream
          else
            return $ WriteFailed WrongExpectedVersion

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
