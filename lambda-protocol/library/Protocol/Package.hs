{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
--------------------------------------------------------------------------------
-- |
-- Module : Protocol.Package
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Protocol.Package where

--------------------------------------------------------------------------------
import Lambda.Prelude
import Data.Serialize
import Data.UUID
import Data.UUID.V4

--------------------------------------------------------------------------------
newtype Cmd = Cmd Word8 deriving (Eq, Ord, Enum, Num, Show)

--------------------------------------------------------------------------------
instance Serialize Cmd where
  put (Cmd w) = putWord8 w
  get = Cmd <$> getWord8

--------------------------------------------------------------------------------
newtype PkgId = PkgId UUID deriving (Eq, Ord, Hashable)

--------------------------------------------------------------------------------
instance Show PkgId where
  show (PkgId pid) = [i|[#{pid}]|]

--------------------------------------------------------------------------------
freshPkgId :: MonadIO m => m PkgId
freshPkgId = liftIO (PkgId <$> nextRandom)

--------------------------------------------------------------------------------
instance Serialize PkgId where
  put (PkgId uuid) =
    putLazyByteString $ toByteString uuid

  get = do
    bs <- getLazyByteString 16
    case fromByteString bs of
      Just uuid -> return $ PkgId uuid
      Nothing   -> mzero

--------------------------------------------------------------------------------
newtype PkgPrefix = PkgPrefix Word32 deriving Num

--------------------------------------------------------------------------------
pkgPrefixIntegral :: Integral a => PkgPrefix -> a
pkgPrefixIntegral (PkgPrefix w) = fromIntegral w

--------------------------------------------------------------------------------
instance Serialize PkgPrefix where
  put (PkgPrefix w) = putWord32le w
  get = PkgPrefix <$> getWord32le

--------------------------------------------------------------------------------
data Pkg =
  Pkg { pkgCmd     :: Cmd
      , pkgId      :: PkgId
      , pkgPayload :: ByteString
      } deriving Eq

--------------------------------------------------------------------------------
pkgPrefix :: Pkg -> PkgPrefix
pkgPrefix Pkg{..} = PkgPrefix (1 + 16 + (fromIntegral $ length pkgPayload))

--------------------------------------------------------------------------------
instance Serialize Pkg where
  put pkg = do
    put $ pkgPrefix pkg
    put $ pkgCmd pkg
    put $ pkgId pkg
    putByteString $ pkgPayload pkg

  get =
    Pkg <$> get
        <*> get
        <*> (remaining >>= getBytes)

--------------------------------------------------------------------------------
instance Show Pkg where
  show Pkg{..} = [i|Pkg #{pkgId} #{pkgCmd}|]

--------------------------------------------------------------------------------
heartbeatRequest :: IO Pkg
heartbeatRequest = do
  pid <- freshPkgId
  return Pkg { pkgCmd     = 0x01
             , pkgId      = pid
             , pkgPayload = mempty
             }

--------------------------------------------------------------------------------
heartbeatResponse :: PkgId -> Pkg
heartbeatResponse pid =
  Pkg { pkgCmd     = 0x02
      , pkgId      = pid
      , pkgPayload = mempty
      }
