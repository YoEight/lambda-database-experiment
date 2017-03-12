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
  , LogType(..)
  , Log(..)
  , newBackend
  , sourceLogs
  ) where

--------------------------------------------------------------------------------
import System.IO hiding (print, putStrLn)

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource
import Data.Serialize hiding (get, put)
import Data.ByteString (hGetSome)
import Data.Conduit
import Data.Conduit.List (sourceList)
import Data.Acquire
import Protocol.Types
import System.Directory

--------------------------------------------------------------------------------
import Server.Messages
import Server.Messaging
import Server.Types

--------------------------------------------------------------------------------
data Backend =
  Backend { _dbPath   :: FilePath
          , _dbPush   :: SomePublisher
          }

--------------------------------------------------------------------------------
newBackend :: (Subscribe sub, Publish pub)
           => FilePath
           -> sub
           -> pub
           -> IO ()
newBackend path sub pub = do
  let b = Backend path (asPublisher pub)

  subscribe_ sub (onSystemInit b)
  subscribe_ sub (onWritePrepares b)

--------------------------------------------------------------------------------
onSystemInit :: Backend -> SystemInit -> IO ()
onSystemInit Backend{..} _ = publish _dbPush (Initialized TransactionLogService)

--------------------------------------------------------------------------------
writeLogEntry :: Handle -> Log -> IO Int
writeLogEntry h entry = do
  pos <- hTell h
  hPut h $ encode (0 :: Int)
  start <- hTell h
  hPut h (encode entry)
  hFlush h
  end <- hTell h
  hSeek h AbsoluteSeek (start - headerSize)
  let siz = fromIntegral (end - start) :: Int
  hPut h (encode siz)
  hFlush h
  hSeek h AbsoluteSeek end
  fromIntegral <$> hTell h

--------------------------------------------------------------------------------
onWritePrepares :: Backend -> WritePrepares -> IO ()
onWritePrepares Backend{..} WritePrepares{..} = runResourceT $ do
  (_, h) <- allocate (openBinaryFile _dbPath ReadWriteMode) hClose
  let action = traverse (onEvent h) preparesEvents
  xs <- evalStateT action (1 :: Int)
  liftIO $ publish _dbPush (WritePrepared preparesId xs)
  where
    len = length preparesEvents

    onEvent h event = do
      i <- get
      let isFirst  = i == 0
          isLast   = i == len
          initFlag = if isFirst
                     then transactionBeginFlag
                     else noopFlag

          flag = if isLast
                 then initFlag * transactionEndFlag
                 else initFlag

          entry = Log { logType        = Prepare
                      , logTransaction = preparesId
                      , logFlag        = flag
                      , logEventId     = eventId event
                      , logStream      = preparesName
                      , logData        = encode event
                      }

      pos <- liftIO $ writeLogEntry h entry
      return $ Prepared { preparedEventId = eventId event
                        , preparedPos     = pos
                        }

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
data LogType = Prepare | Commit deriving (Show, Eq, Enum, Generic)

--------------------------------------------------------------------------------
instance Serialize LogType

--------------------------------------------------------------------------------
data Log =
  Log { logType        :: LogType
      , logTransaction :: Guid
      , logEventId     :: EventId
      , logFlag        :: LogFlag
      , logStream      :: StreamName
      , logData        :: ByteString
      } deriving (Show, Generic)

--------------------------------------------------------------------------------
headerSize :: Num a => a
headerSize = 8

--------------------------------------------------------------------------------
instance Serialize Log

--------------------------------------------------------------------------------
sourceLogs :: MonadResource m => FilePath -> Source m Log
sourceLogs file = bracketP openHandle hClose sourceFromFile
  where
    openHandle = openBinaryFile file ReadMode

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
