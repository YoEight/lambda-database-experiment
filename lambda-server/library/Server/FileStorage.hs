--------------------------------------------------------------------------------
-- |
-- Module : Server.FileStorage
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.FileStorage
  ( FileStorage
  , newFileStorage
  , Flag(..)
  , Flags
  , hasFlag
  , addFlag
  , withFlags
  , Prepare(..)
  , Commit(..)
  , currentPosition
  , writePrepare
  , writeCommit
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Serialize
import Protocol.Types

--------------------------------------------------------------------------------
data FileStorage =
  FileStorage { _file :: FilePath }

--------------------------------------------------------------------------------
newFileStorage :: FilePath -> IO FileStorage
newFileStorage path = return $ FileStorage path

--------------------------------------------------------------------------------
data Flag
  = TransactionBegin
  | TransactionEnd
  deriving (Eq, Ord, Enum, Bounded, Generic)

--------------------------------------------------------------------------------
type Flags = Set Flag

--------------------------------------------------------------------------------
hasFlag :: Flag -> Flags -> Bool
hasFlag f fs = member f fs

--------------------------------------------------------------------------------
addFlag :: Flag -> Flags -> Flags
addFlag f fs = insertSet f fs

--------------------------------------------------------------------------------
withFlags :: (Flag -> Bool) -> Flags
withFlags k = fromList $ filter k [minBound..]

--------------------------------------------------------------------------------
data Prepare =
  Prepare { prepareTransPos    :: Int
          , preparePos         :: Int
          , prepareFlags       :: Flags
          , prepareExpVer      :: EventNumber
          , prepareStream      :: StreamName
          , prepareEventId     :: EventId
          , prepareCorrId      :: Guid
          , prepareEventType   :: EventType
          , prepareData        :: Data
          , prepareMetadata    :: Maybe Properties
          } deriving (Generic)

--------------------------------------------------------------------------------
instance Serialize Prepare

--------------------------------------------------------------------------------
data Commit =
  Commit { commitTransPos    :: Integer
         , commitEventNumber :: EventNumber
         , commitCorrId      :: Guid
         } deriving (Generic)

--------------------------------------------------------------------------------
instance Serialize Commit

--------------------------------------------------------------------------------
currentPosition :: FileStorage -> IO Int
currentPosition FileStorage{..} = runResourceT go
  where
    go = do
      h <- allocate (openBinaryFile _file AppendMode) hClose
      liftIO $ fmap fromIntegral $ hTell h

--------------------------------------------------------------------------------
writePrepare :: FileStorage -> Prepare -> IO Integer
writePrepare FileStorage{..} prepare = runResourceT go
  where
    go = do
      h <- allocate (openBinaryFile _file AppendMode) hClose
      liftIO $ do
        hPut h (encode prepare)
        hTell h

--------------------------------------------------------------------------------
writeCommit :: FileStorage -> Commit -> IO Integer
writeCommit FileStorage{..} commit = runResourceT go
  where
    go = do
      h <- allocate (openBinaryFile _file AppendMode) hClose
      liftIO $ do
        hPut h (encode commit)
        hTell h