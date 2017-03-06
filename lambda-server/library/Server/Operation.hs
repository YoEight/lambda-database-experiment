{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
--------------------------------------------------------------------------------
-- |
-- Module : Server.Operation
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Operation
  ( newOperationExec ) where

--------------------------------------------------------------------------------
import Data.Typeable

--------------------------------------------------------------------------------
import ClassyPrelude
import Protocol.Message
import Protocol.Operation
import Protocol.Types

--------------------------------------------------------------------------------
import Server.Messages
import Server.Messaging
import Server.Settings
import Server.Storage
import Server.Types

--------------------------------------------------------------------------------
data OperationExec =
  OperationExec { _pub :: SomePublisher
                , _ref :: IORef Requests
                }

--------------------------------------------------------------------------------
instance Publish OperationExec where
  publish OperationExec{..} = publish _pub

--------------------------------------------------------------------------------
type Requests = Map Guid SomeOp

--------------------------------------------------------------------------------
takeReq :: Typeable a => Guid -> Requests -> Maybe (Requests, Operation a)
takeReq rid m = do
  op <- lookup rid m
  case op of
    SomeOp o -> (deleteMap rid m,) <$> cast o

--------------------------------------------------------------------------------
insertReq :: Typeable a
          => Guid
          -> Operation a
          -> Requests
          -> Requests
insertReq rid op m =
  insertMap rid (SomeOp op) m

--------------------------------------------------------------------------------
data SomeOp = forall a. Typeable a => SomeOp (Operation a)

--------------------------------------------------------------------------------
newOperationExec :: (Subscribe sub, Publish pub)
                 => Settings
                 -> sub
                 -> pub
                 -> IO ()
newOperationExec setts sub pub = do
  newInMemoryStorage setts sub pub

  op <- OperationExec (asPublisher pub) <$> newIORef mempty

  subscribe_ sub (onOperation op)
  subscribe_ sub (onStorageResp op)

--------------------------------------------------------------------------------
registerOp :: Typeable a => OperationExec -> Operation a -> IO Guid
registerOp OperationExec{..} op = do
  rid <- freshId
  atomicModifyIORef' _ref $ \m ->
    (insertReq rid op m, ())
  return rid

--------------------------------------------------------------------------------
retrieveOp :: forall a. Typeable a
           => OperationExec
           -> Guid
           -> IO (Maybe (Operation a))
retrieveOp OperationExec{..} rid =
  atomicModifyIORef' _ref $ \m ->
    case takeReq rid m of
      Just (m', op) -> (m', Just op)
      Nothing       -> (m, Nothing)

--------------------------------------------------------------------------------
onOperation :: OperationExec -> SomeOperation -> IO ()
onOperation ex (SomeOperation op) = do
  rid <- registerOp ex op
  let tpe =
        case operationType op of
          WriteEvents name ver events ->
            StorageAppendStream name ver events
          ReadEvents name batch ->
            StorageReadStream name batch
  publish ex (StorageReqMsg rid tpe)

--------------------------------------------------------------------------------
onStorageResp :: OperationExec -> StorageRespMsg -> IO ()
onStorageResp ex (StorageRespMsg rid respTpe) =
  case respTpe of
    WriteResult w -> do
      res <- retrieveOp ex rid
      for_ res $ \op -> do
        let tpe =
              case w of
                WriteOk num   -> WriteEventsResp num WriteSuccess
                WriteFailed e ->
                  let flag =
                        case e of
                          WrongExpectedVersion ->
                            WriteWrongExpectedVersion in
                  WriteEventsResp (-1) flag
            pkg = createRespPkg op tpe
        publish ex (TcpSend pkg)
    ReadResult name r -> do
      res <- retrieveOp ex rid
      for_ res $ \op -> do
        let tpe =
              case r of
                ReadOk num eos xs ->
                  ReadEventsResp name xs ReadSuccess num eos
                ReadFailed e ->
                  let flag =
                        case e of
                          StreamNotFound -> ReadNoStream in
                  ReadEventsResp name [] flag (-1) True
            pkg = createRespPkg op tpe
        publish ex (TcpSend pkg)
