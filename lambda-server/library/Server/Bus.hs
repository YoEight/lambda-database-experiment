{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
--------------------------------------------------------------------------------
-- |
-- Module : Server.Bus
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Bus
  ( Bus
  , newBus
  , subscribe
  , publish
  , toMsg
  , fromMsg
  ) where

--------------------------------------------------------------------------------
import Data.Typeable
import Data.Typeable.Internal

--------------------------------------------------------------------------------
import ClassyPrelude

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
  hashWithSalt i (Type _ (Fingerprint b l)) = hashWithSalt i (b, l)

--------------------------------------------------------------------------------
data GetType
  = forall a. Typeable a => FromTypeable a
  | forall a. Typeable a => FromProxy (Proxy a)


--------------------------------------------------------------------------------
getType :: GetType -> Type
getType (FromTypeable a) = let t@(TypeRep fp _ _ _) = typeOf a in Type t fp
getType (FromProxy prx)  = let t@(TypeRep fp _ _ _) = typeRep prx in Type t fp

--------------------------------------------------------------------------------
data HandlerK = forall a. Typeable a => HandlerK (Proxy a) (a -> IO ())

--------------------------------------------------------------------------------
instance Show HandlerK where
  show (HandlerK prx _) = "Handle " <> show (typeRep prx)

--------------------------------------------------------------------------------
data Bus =
  Bus { _busHandlers :: IORef (HashMap Type (Seq HandlerK)) }

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
messageType :: Type
messageType = getType (FromProxy (Proxy :: Proxy Message))

--------------------------------------------------------------------------------
newBus :: IO Bus
newBus = Bus <$> newIORef mempty

--------------------------------------------------------------------------------
subscribe :: forall a. Typeable a => Bus -> (a -> IO ()) -> IO ()
subscribe Bus{..} k = atomicModifyIORef' _busHandlers $ \m ->
  let tpe  = getType (FromProxy (Proxy :: Proxy a))
      hdl  = HandlerK Proxy k
      next = alterMap $ \input ->
        case input of
          Nothing -> Just (singleton hdl)
          Just hs -> Just (snoc hs hdl) in
  (next tpe m, ())

--------------------------------------------------------------------------------
publish :: Typeable a => Bus -> a -> IO ()
publish Bus{..} a = do
  m <- readIORef _busHandlers
  let tpe = getType (FromTypeable a)

  for_ (lookup tpe m) $ \hs ->
    for_ hs $ \(HandlerK _ k) -> do
      let Just b = cast a
      k b

  unless (tpe == messageType) $ do
    for_ (lookup messageType m) $ \hs ->
      for_ hs $ \(HandlerK _ k) -> do
        let Just b = cast (Message a)
        k b
