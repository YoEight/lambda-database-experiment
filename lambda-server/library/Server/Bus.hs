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
  , toMsg
  , fromMsg
  ) where

--------------------------------------------------------------------------------
import Data.Typeable
import Data.Typeable.Internal

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Sequence (ViewL(..), viewl)
import Protocol.Types hiding (singleton)

--------------------------------------------------------------------------------
import Server.Messaging

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
type Handlers = HashMap Type (Seq HandlerK)

--------------------------------------------------------------------------------
unregisterTrigger :: forall a. Typeable a => Trigger a -> Handlers -> Handlers
unregisterTrigger tgr = go (getType (FromProxy (Proxy :: Proxy a)))
  where
    go = adjustMap $ \seq ->
      let _F agg cur =
            case viewl cur of
              EmptyL -> agg
              h@(HandlerK tid _) :< rest
                | tgrId tid == tgrId tgr -> agg <> rest
                | otherwise              -> _F (snoc agg h) rest in
      _F mempty seq

--------------------------------------------------------------------------------
data HandlerK =
  forall a. Typeable a =>
  HandlerK { _handleTrigger :: Trigger a
           , _handlerKey    :: a -> IO ()
           }

--------------------------------------------------------------------------------
instance Show HandlerK where
  show (HandlerK (_ :: Trigger a) _) =
    "Handle " <> show (typeRep (Proxy :: Proxy a))

--------------------------------------------------------------------------------
data Bus =
  Bus { _busName     :: Text
      , _busHandlers :: IORef (HashMap Type (Seq HandlerK))
      }

--------------------------------------------------------------------------------
messageType :: Type
messageType = getType (FromProxy (Proxy :: Proxy Message))

--------------------------------------------------------------------------------
newBus :: Text -> IO Bus
newBus name = Bus name <$> newIORef mempty

--------------------------------------------------------------------------------
instance Subscribe Bus where
  subscribe   = _subscribe
  unsubscribe = _unsubscribe

--------------------------------------------------------------------------------
instance Publish Bus where
  publish = _publish

--------------------------------------------------------------------------------
_subscribe :: forall a. Typeable a => Bus -> (a -> IO ()) -> IO (Trigger a)
_subscribe Bus{..} k = do
  tid <- freshId
  atomicModifyIORef' _busHandlers $ \m ->
    let tpe  = getType (FromProxy (Proxy :: Proxy a))
        hdl  = HandlerK tid k
        next = alterMap $ \input ->
          case input of
            Nothing -> Just (singleton hdl)
            Just hs -> Just (snoc hs hdl) in
    (next tpe m, ())
  return tid

--------------------------------------------------------------------------------
_unsubscribe :: Typeable a => Bus -> Trigger a -> IO ()
_unsubscribe Bus{..} tgr =
  atomicModifyIORef' _busHandlers $ \m -> (unregisterTrigger tgr m, ())

--------------------------------------------------------------------------------
_publish :: Typeable a => Bus -> a -> IO ()
_publish Bus{..} a = do
  m <- readIORef _busHandlers
  let tpe = getType (FromTypeable a)

  for_ (lookup tpe m) $ \hs ->
    for_ hs $ \(HandlerK _ k) -> do
      let Just b = cast a
      k b

  unless (tpe == messageType) $ do
    for_ (lookup messageType m) $ \hs ->
      for_ hs $ \(HandlerK _ k) -> do
        let Just b = cast (toMsg a)
        k b
