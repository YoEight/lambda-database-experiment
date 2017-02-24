{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
--------------------------------------------------------------------------------
-- |
-- Module : Server.RequestId
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.RequestId where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.UUID
import Data.UUID.V4

--------------------------------------------------------------------------------
newtype RequestId a = RequestId UUID deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
freshRequestId :: MonadIO m => m (RequestId a)
freshRequestId = liftIO $ fmap RequestId nextRandom

--------------------------------------------------------------------------------
data SomeRequestId = forall a. Typeable a => SomeRequestId (RequestId a)

--------------------------------------------------------------------------------
instance Eq SomeRequestId where
  sa == sb =
    case (sa, sb) of
      (SomeRequestId (RequestId a), SomeRequestId (RequestId b)) -> a == b

--------------------------------------------------------------------------------
instance Ord SomeRequestId where
  compare sa sb =
    case (sa, sb) of
      (SomeRequestId (RequestId a), SomeRequestId (RequestId b)) -> compare a b
