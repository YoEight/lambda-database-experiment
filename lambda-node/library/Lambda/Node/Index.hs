--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Node.Index
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Index
  ( Indexer
  , newIndexer
  , indexEvents
  , readStreamEvents
  , nextStreamEventNum
  ) where

--------------------------------------------------------------------------------
import Lambda.Prelude
import Protocol.Types

--------------------------------------------------------------------------------
import qualified Lambda.Node.Journal as Journal
import           Lambda.Node.Settings

--------------------------------------------------------------------------------
data IndexKey
  = StreamKey StreamName
  | StreamNumKey StreamName
    deriving (Eq, Ord, Show, Generic)

--------------------------------------------------------------------------------
instance Hashable IndexKey

--------------------------------------------------------------------------------
type SeekPos = Integer

--------------------------------------------------------------------------------
data IndexValue
  = StreamEvents (Seq SeekPos)
  | StreamNextNum EventNumber

--------------------------------------------------------------------------------
type IndexMap = HashMap IndexKey IndexValue

--------------------------------------------------------------------------------
insertStreamEventPos :: StreamName -> Seq SeekPos -> IndexMap -> IndexMap
insertStreamEventPos name pos = alterMap go (StreamKey name)
  where
    go Nothing    = Just (StreamEvents pos)
    go (Just val) =
      let StreamEvents old = val
       in Just $ StreamEvents (old <> pos)

--------------------------------------------------------------------------------
lookupStreamEventPos :: StreamName -> IndexMap -> Maybe [SeekPos]
lookupStreamEventPos name = fmap go . lookup (StreamKey name)
  where
    go val =
      let StreamEvents pos = val
       in toList pos

--------------------------------------------------------------------------------
insertStreamNextNum :: StreamName -> EventNumber -> IndexMap -> IndexMap
insertStreamNextNum name num = insertMap (StreamNumKey name) (StreamNextNum num)

--------------------------------------------------------------------------------
lookupStreamNextNum :: StreamName -> IndexMap -> Maybe EventNumber
lookupStreamNextNum name = fmap go . lookup (StreamNumKey name)
  where
    go val =
      let StreamNextNum num = val
       in num

--------------------------------------------------------------------------------
data Indexer =
  Indexer { _map     :: IORef IndexMap
          , _journal :: Journal.InMemory
          }

--------------------------------------------------------------------------------
newIndexer :: Lambda Settings Indexer
newIndexer = Indexer <$> newIORef mempty
                     <*> Journal.newInMemory

--------------------------------------------------------------------------------
indexEvents :: Indexer
            -> StreamName
            -> [Event]
            -> Lambda Settings EventNumber
indexEvents self name xs = do
  mnum <- lookupStreamNextNum name <$> readIORef (_map self)

  let num  = fromMaybe 0 mnum
      next = num + len

  poss <- Journal.runInMemory (_journal self) (foldM persist mempty $ indexed num)

  atomicModifyIORef (_map self) $ \m ->
    let m' = insertStreamNextNum name next $
             insertStreamEventPos name poss m
     in (m', next)
  where
    len = fromIntegral $ length xs

    indexed from = zip [from..] xs

    persist acc (num, evt) =
      fmap (acc `snoc`) (Journal.marshal (SavedEvent num evt))

--------------------------------------------------------------------------------
nextStreamEventNum :: Indexer -> StreamName -> Lambda Settings EventNumber
nextStreamEventNum self name = do
  mnum <- lookupStreamNextNum name <$> readIORef (_map self)
  return $ fromMaybe 0 mnum

--------------------------------------------------------------------------------
readStreamEvents :: Indexer
                 -> StreamName
                 -> Lambda Settings (Maybe [SavedEvent])
readStreamEvents self name = do
  m  <- readIORef (_map self)

  let mpos   = lookupStreamEventPos name m
      action = traverse (fmap toList . traverse readStream) mpos

  Journal.runInMemory (_journal self) action
  where
    readStream pos = do
      Journal.setPos pos
      Journal.unmarshal
