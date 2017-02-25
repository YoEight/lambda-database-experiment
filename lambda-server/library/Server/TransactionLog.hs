{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
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
  , LogType(..)
  , Log(..)
  , save
  , newBackend
  , logs
  , initializeHeader
  , getLastSeqNum
  , newTransactionId
  ) where

--------------------------------------------------------------------------------
import System.IO hiding (print)

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
  = Prepared TransactionId EventId Int
  | Committed Int TransactionId

--------------------------------------------------------------------------------
data Msg
  = DoSave StreamName [Event]
  | DoCommit TransactionId

--------------------------------------------------------------------------------
instance Serialize TransactionId where
  get = do
    bs <- get
    case fromByteString bs of
      Just uuid -> return $ TransactionId uuid
      Nothing   -> mzero

  put (TransactionId uuid) = put (toByteString uuid)

--------------------------------------------------------------------------------
newTransactionId :: IO TransactionId
newTransactionId = TransactionId <$> nextRandom

--------------------------------------------------------------------------------
data Backend =
  Backend { _dbName   :: FilePath
          , _dbPush   :: Publish LogMsg
          , _dbChan   :: Chan Msg
          , _dbSeqNum :: IORef Int
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

  action
  return b

--------------------------------------------------------------------------------
save :: Backend -> StreamName -> [Event] -> IO ()
save Backend{..} n xs = writeChan _dbChan (DoSave n xs)

--------------------------------------------------------------------------------
logs :: MonadResource m => Backend -> Source m Log
logs Backend{..} = sourceLogs _dbName

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

  let entry = Log { logSeqNum      = cur
                  , logType        = Commit
                  , logTransaction = tid
                  , logFlag        = noopFlag
                  , logStream      = ""
                  , logData        = ""
                  }

  runResourceT (yield entry $$ sinkLogs b)

  publish _dbPush (Committed cur tid)

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
data LogType = Prepare | Commit deriving (Show, Eq)

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
data Log =
  Log { logSeqNum      :: Int
      , logType        :: LogType
      , logTransaction :: TransactionId
      , logFlag        :: LogFlag
      , logStream      :: StreamName
      , logData        :: ByteString
      } deriving Show

--------------------------------------------------------------------------------
headerSize :: Num a => a
headerSize = 8

--------------------------------------------------------------------------------
instance Serialize Log where
  get =
    Log <$> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get

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

          entry  = Log { logSeqNum      = cur
                       , logType        = Prepare
                       , logTransaction = tid
                       , logFlag        = flag
                       , logStream      = name
                       , logData        = encode e
                       }

      yield entry
      liftIO $ publish _dbPush (Prepared tid eventId cur)
      go False next

--------------------------------------------------------------------------------
sourceLogs :: MonadResource m => FilePath -> Source m Log
sourceLogs file = bracketP openHandle hClose sourceFromFile
  where
    openHandle = do
      h <- openBinaryFile file ReadMode
      hSeek h AbsoluteSeek headerSize
      return h

--------------------------------------------------------------------------------
sourceFromFile :: MonadResource m => Handle -> Source m Log
sourceFromFile h = go
  where
    go = do
      bs <- liftIO $ hGetSome h headerSize
      unless (null bs) $
        case decode bs of
          Left _    -> fail "Wrong line framing"
          Right siz -> do
            dat <- liftIO $ hGetSome h siz
            case decode dat of
              Left e      -> fail $ show e ++ ": Wrong log format"
              Right entry -> do
                yield entry
                go

--------------------------------------------------------------------------------
sinkLogs :: MonadResource m => Backend -> Sink Log m ()
sinkLogs b = bracketP useFile hClose (consumeToFile b)
  where
    useFile = do
      h <- openBinaryFile (_dbName b) ReadWriteMode
      hSeek h SeekFromEnd 0
      return h

--------------------------------------------------------------------------------
consumeToFile :: MonadResource m => Backend -> Handle -> Sink Log m ()
consumeToFile Backend{..} h = await >>= go
  where
    go Nothing = liftIO $ do
      cur <- readIORef _dbSeqNum
      hSeek h AbsoluteSeek 0
      hPut h $ encode cur
      hFlush h
    go (Just entry) = do
      liftIO $ do
        hPut h $ encode (0 :: Int)
        start <- hTell h
        hPut h (encode entry)
        hFlush h
        end <- hTell h
        hSeek h AbsoluteSeek (start - headerSize)
        hPut h $ encode (fromIntegral (end-start) :: Int)
        hFlush h
        hSeek h AbsoluteSeek end

      await >>= go

--------------------------------------------------------------------------------
loadLastSeqNum :: (MonadCatch m, MonadResource m) => FilePath -> m Int
loadLastSeqNum path = do
  (_, h) <- allocate (openBinaryFile path ReadWriteMode) hClose

  catchIOError (getLastSeqNum h) $ \e ->
    if isDoesNotExistError e || isUserError e
    then 0 <$ initializeHeader h 0
    else throw e

--------------------------------------------------------------------------------
getLastSeqNum :: MonadIO m => Handle -> m Int
getLastSeqNum h = liftIO $ do
  hSeek h AbsoluteSeek 0
  bs <- hGetSome h headerSize
  case decode bs of
    Right i -> return i
    Left  e -> fail e

--------------------------------------------------------------------------------
initializeHeader :: MonadIO m => Handle -> Int -> m ()
initializeHeader h header = liftIO $ do
  hSeek h AbsoluteSeek 0
  hPut h (encode header)
  hFlush h
