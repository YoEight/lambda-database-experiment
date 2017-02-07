{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
--------------------------------------------------------------------------------
-- |
-- Module : Server.Backend
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Backend where

--------------------------------------------------------------------------------
import System.IO

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.Trans.Resource
import Data.Serialize
import Data.ByteString (hGetSome)
import Data.Conduit
import Data.UUID hiding (null)

--------------------------------------------------------------------------------
data LogFlag
  = TransactionBegin UUID
  | TransactionEnd UUID
  | NoFlag

--------------------------------------------------------------------------------
logFlagSize :: LogFlag -> Int
logFlagSize TransactionBegin{} = 1 + 16
logFlagSize TransactionEnd{}   = 1 + 16
logFlagSize NoFlag             = 1

--------------------------------------------------------------------------------
instance Serialize LogFlag where
  get = do
    tpe <- getWord8
    case tpe of
      0x00 -> return NoFlag
      0x01 -> do
        bs <- getByteString 16
        case fromByteString $ fromStrict bs of
          Just uuid -> return $ TransactionBegin uuid
      0x02 -> do
        bs <- getByteString 16
        case fromByteString $ fromStrict bs of
          Just uuid -> return $ TransactionEnd uuid

  put (TransactionBegin uuid) = do
    putWord8 0x01
    put $ toByteString uuid
  put (TransactionEnd uuid) = do
    putWord8 0x02
    put $ toByteString uuid
  put NoFlag = putWord8 0x00

--------------------------------------------------------------------------------
data Log =
  Log { logSeqNum :: Int
      , logFlag   :: LogFlag
      , logData   :: ByteString
      }

--------------------------------------------------------------------------------
logSize :: Log -> Int
logSize Log{..} = 1 + logFlagSize logFlag + length logData

--------------------------------------------------------------------------------
instance Serialize Log where
  get =
    Log <$> get
        <*> get
        <*> (remaining >>= getByteString)

  put Log{..} = do
    put logSeqNum
    put logFlag
    put logData

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
