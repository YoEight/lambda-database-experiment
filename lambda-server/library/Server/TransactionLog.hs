{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
--------------------------------------------------------------------------------
-- |
-- Module : Server.TransactionLog
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.TransactionLog
  ( Backend
  , LogMsg(..)
  , TransactionId
  , save
  ) where

--------------------------------------------------------------------------------
import System.IO

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.Trans.Resource
import Data.Serialize
import Data.ByteString (hGetSome)
import Data.Conduit
import Data.Conduit.List (sourceList)
import Data.UUID hiding (null)
import Data.UUID.V4
import Protocol.Types

--------------------------------------------------------------------------------
import Server.Messaging

--------------------------------------------------------------------------------
newtype TransactionId = TransactionId UUID deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
data LogMsg
  = Prepared TransactionId EventId Int32
  | Committed TransactionId

--------------------------------------------------------------------------------
data Msg
  = DoSave StreamName [Event]
  | DoCommit TransactionId

--------------------------------------------------------------------------------
instance Serialize TransactionId where
  get = do
    bs <- getLazyByteString 16
    case fromByteString bs of
      Just uuid -> return $ TransactionId uuid
      Nothing   -> mzero

  put (TransactionId uuid) =
    put (toByteString uuid)

--------------------------------------------------------------------------------
newTransactionId :: IO TransactionId
newTransactionId = TransactionId <$> nextRandom

--------------------------------------------------------------------------------
transactionIdSize :: Int
transactionIdSize = 16

--------------------------------------------------------------------------------
data Backend =
  Backend { _dbName   :: FilePath
          , _dbPush   :: Publish LogMsg
          , _dbChan   :: Chan Msg
          , _dbSeqNum :: IORef Int32
          }

--------------------------------------------------------------------------------
newBackend :: FilePath -> Publish LogMsg -> IO Backend
newBackend path pub = do
  let seqNumEff       = runResourceT $ loadLastSeqNum path
      createSeqNumRef = seqNumEff >>= newIORef

  c <- newChan
  b <- Backend path pub c <$> createSeqNumRef

  let action = do
        _ <- forkFinally (worker b) $ \_ -> action
        return ()

  return b

--------------------------------------------------------------------------------
save :: Backend -> StreamName -> [Event] -> IO ()
save Backend{..} n xs = writeChan _dbChan (DoSave n xs)

--------------------------------------------------------------------------------
worker :: Backend -> IO ()
worker b@Backend{..} = forever (readChan _dbChan >>= go)
  where
    go (DoSave n xs) = doSave b n xs
    go (DoCommit t)  = doCommit b t

--------------------------------------------------------------------------------
doSave :: Backend -> StreamName -> [Event] -> IO ()
doSave b@Backend{..} name evts = do
  tid <- newTransactionId

  let src = sourceList evts $= eventToLog b tid name
  runResourceT (src $$ sinkLogs b)

--------------------------------------------------------------------------------
doCommit :: Backend -> TransactionId -> IO ()
doCommit b@Backend{..} tid = do
  cur <- liftIO $ atomicModifyIORef' _dbSeqNum $ \i -> (i+1, i)

  let log = Log { logSeqNum      = cur
                , logType        = Commit
                , logTransaction = tid
                , logFlag        = noopFlag
                , logStream      = ""
                , logData        = ""
                }

  runResourceT (yield log $$ sinkLogs b)

  publish _dbPush (Committed tid)

--------------------------------------------------------------------------------
type LogFlag = Word8

--------------------------------------------------------------------------------
noopFlag :: Word8
noopFlag = 0x02

--------------------------------------------------------------------------------
transactionBeginFlag :: Word8
transactionBeginFlag = 0x04

--------------------------------------------------------------------------------
transactionEndFlag :: Word8
transactionEndFlag = 0x08

--------------------------------------------------------------------------------
logFlagSize :: Int
logFlagSize = 1

--------------------------------------------------------------------------------
data LogType = Prepare | Commit

--------------------------------------------------------------------------------
instance Serialize LogType where
  get = do
    w <- getWord8
    case w of
      0x00 -> return Prepare
      0x01 -> return Commit
      _    -> mzero

  put Prepare = putWord8 0x00
  put Commit  = putWord8 0x01

--------------------------------------------------------------------------------
logTypeSize :: Int
logTypeSize = 1

--------------------------------------------------------------------------------
data Log =
  Log { logSeqNum      :: Int32
      , logType        :: LogType
      , logTransaction :: TransactionId
      , logFlag        :: LogFlag
      , logStream      :: StreamName
      , logData        :: ByteString
      }

--------------------------------------------------------------------------------
streamNameSize :: StreamName -> Int
streamNameSize (StreamName n) = length n

--------------------------------------------------------------------------------
logSize :: Log -> Int
logSize Log{..} = 1 + logTypeSize
                    + transactionIdSize
                    + logFlagSize
                    + streamNameSize logStream
                    + length logData

--------------------------------------------------------------------------------
instance Serialize Log where
  get =
    Log <$> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> (remaining >>= getByteString)

  put Log{..} = do
    put logSeqNum
    put logType
    put logTransaction
    put logFlag
    put logStream
    put logData

--------------------------------------------------------------------------------
eventToLog :: MonadIO m
           => Backend
           -> TransactionId
           -> StreamName
           -> Conduit Event m Log
eventToLog Backend{..} tid name = await >>= go True
  where
    go _ Nothing                  = liftIO $ writeChan _dbChan (DoCommit tid)
    go isFirst (Just e@Event{..}) = do
      next <- await
      cur  <- liftIO $ atomicModifyIORef' _dbSeqNum $ \i -> (i+1, i)
      let tmp  = if isFirst
                 then transactionBeginFlag
                 else noopFlag

          flag = if isNothing next
                 then tmp * transactionEndFlag
                 else tmp

          log  = Log { logSeqNum      = cur
                     , logType        = Prepare
                     , logTransaction = tid
                     , logFlag        = flag
                     , logStream      = name
                     , logData        = encode e
                     }

      yield log
      publish _dbPush (Prepared tid eventId cur)
      go False next

--------------------------------------------------------------------------------
sourceLogs :: MonadResource m => FilePath -> Source m Log
sourceLogs file = bracketP openHandle hClose sourceFromFile
  where
    openHandle = do
      h <- openBinaryFile file ReadMode
      hSeek h AbsoluteSeek 4
      return h

--------------------------------------------------------------------------------
sourceFromFile :: MonadResource m => Handle -> Source m Log
sourceFromFile h = go
  where
    go = do
      bs <- liftIO $ hGetSome h 4
      unless (null bs) $
        case runGet (fromIntegral <$> getWord32le) bs of
          Left _    -> fail "Wrong line framing"
          Right siz -> do
            dat <- liftIO $ hGetSome h siz
            case decode bs of
              Left _    -> fail "Wrong log format"
              Right log -> do
                yield log
                go

--------------------------------------------------------------------------------
sinkLogs :: MonadResource m => Backend -> Sink Log m ()
sinkLogs b =
  bracketP (openBinaryFile (_dbName b) AppendMode) hClose (consumeToFile b)

--------------------------------------------------------------------------------
consumeToFile :: MonadResource m => Backend -> Handle -> Sink Log m ()
consumeToFile Backend{..} h = await >>= go
  where
    go Nothing = liftIO $ do
      cur <- readIORef _dbSeqNum
      hSeek h AbsoluteSeek 0
      hPut h (runPut $ putWord32le (fromIntegral cur))
      hFlush h
    go (Just log) = do
      liftIO $ do
        hPut h (runPut (putWord32le $ fromIntegral $ logSize log))
        hPut h (encode log)
        hFlush h

      await >>= go

--------------------------------------------------------------------------------
loadLastSeqNum :: (MonadCatch m, MonadResource m) => FilePath -> m Int32
loadLastSeqNum path = do
  (_, h) <- allocate (openBinaryFile path ReadWriteMode) hClose

  catchIOError (getLastSeqNum h) $ \e ->
    if isDoesNotExistError e
    then initializeHeader h
    else throw e

--------------------------------------------------------------------------------
getLastSeqNum :: MonadIO m => Handle -> m Int32
getLastSeqNum h = liftIO $ do
  hSeek h AbsoluteSeek 0
  bs <- hGetSome h 4
  case runGet getWord32le bs of
    Right i -> return $ fromIntegral i
    Left  e -> fail e

--------------------------------------------------------------------------------
initializeHeader :: MonadIO m => Handle -> m Int32
initializeHeader h = liftIO $ do
  hSeek h AbsoluteSeek 0
  hPut h (runPut $ putWord32le 0)
  hFlush h
  return 0
