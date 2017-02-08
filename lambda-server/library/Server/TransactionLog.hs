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
module Server.TransactionLog where

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
newtype TransactionId = TransactionId UUID deriving (Eq, Ord, Show)

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
          , _dbLock   :: QSem
          , _dbSeqNum :: IORef Int
          }

--------------------------------------------------------------------------------
newBackend :: FilePath -> IO Backend
newBackend p = Backend p <$> newQSem 1
                         <*> newIORef 0

--------------------------------------------------------------------------------
save :: Backend -> StreamName -> [SavedEvent] -> IO TransactionId
save Backend{..} name evts =
  bracket_ (waitQSem _dbLock) (signalQSem _dbLock) $ do
    tid <- newTransactionId

    let src = sourceList evts $= eventToLog _dbSeqNum tid name
    runResourceT (src $$ sinkLogs _dbName)

    return tid

--------------------------------------------------------------------------------
commit :: Backend -> TransactionId -> IO ()
commit Backend{..} tid = bracket_ (waitQSem _dbLock) (signalQSem _dbLock) $ do
  cur <- liftIO $ atomicModifyIORef' _dbSeqNum $ \i -> (i+1, i)

  let log = Log { logSeqNum      = cur
                , logType        = Commit
                , logTransaction = tid
                , logFlag        = noopFlag
                , logStream      = ""
                , logData        = ""
                }

  runResourceT (yield log $$ sinkLogs _dbName)

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
  Log { logSeqNum      :: Int
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
           => IORef Int
           -> TransactionId
           -> StreamName
           -> Conduit SavedEvent m Log
eventToLog ref tid name = await >>= go True
  where
    go _ Nothing        = return ()
    go isFirst (Just e) = do
      next <- await
      cur  <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i)
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
      go False next

--------------------------------------------------------------------------------
sourceLogs :: MonadResource m => FilePath -> Source m Log
sourceLogs file = bracketP (openFile file ReadMode) hClose sourceFromFile

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
sinkLogs :: MonadResource m => FilePath -> Sink Log m ()
sinkLogs file = bracketP (openFile file AppendMode) hClose consumeToFile

--------------------------------------------------------------------------------
consumeToFile :: MonadResource m => Handle -> Sink Log m ()
consumeToFile h  = awaitForever go
  where
    go log = liftIO $ do
      hPut h (runPut (putWord32le $ fromIntegral $ logSize log))
      hPut h (encode log)
      hFlush h
