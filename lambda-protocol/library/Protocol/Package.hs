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
import ClassyPrelude
import Data.Serialize
import Data.UUID

--------------------------------------------------------------------------------
newtype Cmd = Cmd Word8 deriving (Eq, Ord, Enum, Show)

--------------------------------------------------------------------------------
instance Serialize Cmd where
  put (Cmd w) = putWord8 w
  get = Cmd <$> getWord8

--------------------------------------------------------------------------------
newtype PkgId = PkgId UUID deriving (Eq, Ord, Show)

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
newtype PkgPrefix = PkgPrefix Word32

--------------------------------------------------------------------------------
instance Serialize PkgPrefix where
  put (PkgPrefix w) = putWord32le w
  get = PkgPrefix <$> getWord32le

--------------------------------------------------------------------------------
data Pkg =
  Pkg { pkgCmd     :: Cmd
      , pkgId      :: PkgId
      , pkgPayload :: ByteString
      }

--------------------------------------------------------------------------------
pkgPrefix :: Pkg -> PkgPrefix
pkgPrefix Pkg{..} = PkgPrefix (1 + 16 + (fromIntegral $ length pkgPayload))

--------------------------------------------------------------------------------
instance Serialize Pkg where
  put pkg = do
    put $ pkgPrefix pkg
    put $ pkgCmd pkg
    put $ pkgPayload pkg

  get =
    Pkg <$> get
        <*> get
        <*> (remaining >>= getBytes)
