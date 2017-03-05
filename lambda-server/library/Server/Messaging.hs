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
  , toMsg
  , fromMsg
  , asProvider
  , asPublisher
  ) where

--------------------------------------------------------------------------------
import Data.Typeable

--------------------------------------------------------------------------------
import ClassyPrelude

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
class Subscribe s where
  subscribe :: Typeable a => s -> (a -> IO ()) -> IO ()

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

--------------------------------------------------------------------------------
asProvider :: Subscribe s => s -> SomeProvider
asProvider = SomeProvider

--------------------------------------------------------------------------------
asPublisher :: Publish p => p -> SomePublisher
asPublisher = SomePublisher
