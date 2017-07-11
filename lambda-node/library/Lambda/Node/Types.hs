{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Node.Types
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Types where

--------------------------------------------------------------------------------
import Data.Typeable
import Data.Typeable.Internal

--------------------------------------------------------------------------------
import Lambda.Node.Prelude

--------------------------------------------------------------------------------
-- Messaging
--------------------------------------------------------------------------------
data Message where
  Message :: Typeable a => a -> Message
  deriving Typeable

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
class Pub p where
  publishSTM :: Typeable a => p -> a -> STM Bool

--------------------------------------------------------------------------------
data EventHandler

--------------------------------------------------------------------------------
class Sub s where
  subscribeEventHandler :: s -> EventHandler -> IO ()

--------------------------------------------------------------------------------
data Publish = forall p. Pub p => Publish p

--------------------------------------------------------------------------------
instance Pub Publish where
  publishSTM (Publish p) a = publishSTM p a

--------------------------------------------------------------------------------
data Subscribe = forall p. Sub p => Subscribe p

--------------------------------------------------------------------------------
instance Sub Subscribe where
  subscribeEventHandler (Subscribe p) a = subscribeEventHandler p a

--------------------------------------------------------------------------------
data Hub = forall h. (Sub h, Pub h) => Hub h

--------------------------------------------------------------------------------
instance Sub Hub where
  subscribeEventHandler (Hub h) = subscribeEventHandler h

--------------------------------------------------------------------------------
instance Pub Hub where
  publishSTM (Hub h) = publishSTM h

--------------------------------------------------------------------------------
asSub :: Sub s => s -> Subscribe
asSub = Subscribe

--------------------------------------------------------------------------------
asPub :: Pub p => p -> Publish
asPub = Publish

--------------------------------------------------------------------------------
asHub :: (Sub h, Pub h) => h -> Hub
asHub = Hub

--------------------------------------------------------------------------------
data Type = Type TypeRep Fingerprint

--------------------------------------------------------------------------------
instance Show Type where
  show (Type rep _) = "type " <> show rep

--------------------------------------------------------------------------------
instance Eq Type where
  Type _ a == Type _ b = a == b

--------------------------------------------------------------------------------
instance Ord Type where
  compare (Type _ a) (Type _ b) = compare a b

--------------------------------------------------------------------------------
instance Hashable Type where
  hashWithSalt s (Type _ (Fingerprint b l)) = hashWithSalt s (b, l)

--------------------------------------------------------------------------------
data GetType
  = forall a. Typeable a => FromTypeable a
  | forall prx a. Typeable a => FromProxy (prx a)

--------------------------------------------------------------------------------
getType :: GetType -> Type
getType op = Type t (typeRepFingerprint t)
  where
    t = case op of
          FromTypeable a -> typeOf a
          FromProxy prx  -> typeRep prx