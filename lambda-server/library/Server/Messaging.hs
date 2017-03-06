{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : Server.Messaging
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Messaging
  ( Publish(..)
  , Subscribe(..)
  , Proxy(..)
  , Message(..)
  , SomeProvider
  , SomePublisher
  , Trigger
  , tgrId
  , toMsg
  , fromMsg
  , asProvider
  , asPublisher
  , subscribe_
  ) where

--------------------------------------------------------------------------------
import Data.Typeable

--------------------------------------------------------------------------------
import ClassyPrelude
import Protocol.Types

--------------------------------------------------------------------------------
data Message = forall a. Typeable a => Message a

--------------------------------------------------------------------------------
instance Show Message where
  show (Message a) = "Message: " <> show (typeOf a)

--------------------------------------------------------------------------------
toMsg :: Typeable a => a -> Message
toMsg = Message

--------------------------------------------------------------------------------
fromMsg :: Typeable a => Message -> Maybe a
fromMsg (Message a) = cast a

--------------------------------------------------------------------------------
class Publish p where
  publish :: Typeable a => p -> a -> IO ()

--------------------------------------------------------------------------------
newtype Trigger a = Trigger { tgrId :: Guid } deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
instance FreshId (Trigger a) where
  freshId = Trigger <$> freshId

--------------------------------------------------------------------------------
class Subscribe s where
  subscribe :: Typeable a => s -> (a -> IO ()) -> IO (Trigger a)
  unsubscribe :: Typeable a => s -> Trigger a -> IO ()

--------------------------------------------------------------------------------
data SomePublisher = forall p. Publish p => SomePublisher p

--------------------------------------------------------------------------------
instance Publish SomePublisher where
  publish (SomePublisher p) a = publish p a

--------------------------------------------------------------------------------
data SomeProvider = forall p. Subscribe p => SomeProvider p

--------------------------------------------------------------------------------
instance Subscribe SomeProvider where
  subscribe (SomeProvider p) a = subscribe p a
  unsubscribe (SomeProvider p) g = unsubscribe p g

--------------------------------------------------------------------------------
subscribe_ :: (Typeable a, Subscribe s) => s -> (a -> IO ()) -> IO ()
subscribe_ s k = () <$ subscribe s k

--------------------------------------------------------------------------------
asProvider :: Subscribe s => s -> SomeProvider
asProvider = SomeProvider

--------------------------------------------------------------------------------
asPublisher :: Publish p => p -> SomePublisher
asPublisher = SomePublisher
