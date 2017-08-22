{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Bus.Types
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Bus.Types where

--------------------------------------------------------------------------------
import Data.Semigroup
import Data.Typeable
import GHC.Fingerprint
import Lambda.Prelude

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
data Callback settings where
  Callback :: Typeable a
           => Proxy a
           -> (a -> React settings ())
           -> Callback settings

--------------------------------------------------------------------------------
instance Show (Callback settings) where
  show (Callback prx _) = "Callback expects " <> show (typeRep prx)

--------------------------------------------------------------------------------
class PubSub p where
  subscribeSTM :: p s -> Callback s -> STM ()
  publishSTM   :: Typeable a => p s -> a -> STM Bool

--------------------------------------------------------------------------------
data SomeBus s = forall p. PubSub p => SomeBus (p s)

--------------------------------------------------------------------------------
instance PubSub SomeBus where
  subscribeSTM (SomeBus p) c = subscribeSTM p c
  publishSTM (SomeBus p) a = publishSTM p a

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

--------------------------------------------------------------------------------
messageType :: Type
messageType = getType (FromProxy (Proxy :: Proxy Message))

--------------------------------------------------------------------------------
newtype React settings a =
  React { unReact :: ReaderT (SomeBus settings) (Lambda settings) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadFix
           , MonadLogger
           , MonadBase IO
           , MonadBaseControl IO
           )

--------------------------------------------------------------------------------
publish :: Typeable a => a -> React settings ()
publish a = React $ do
  p <- ask
  _ <- atomically $ publishSTM p a
  return ()

--------------------------------------------------------------------------------
reactSettings :: React settings settings
reactSettings = React $ lift getSettings

--------------------------------------------------------------------------------
runReact :: PubSub p => React s a -> p s -> Lambda s a
runReact (React m) p = runReaderT m (SomeBus p)
