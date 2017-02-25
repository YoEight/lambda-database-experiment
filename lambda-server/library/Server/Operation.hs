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
  ( OperationExec
  , OpMsg(..)
  , newOperationExec
  , executeOperation
  ) where

--------------------------------------------------------------------------------
import Data.Typeable

--------------------------------------------------------------------------------
import ClassyPrelude
import Protocol.Message
import Protocol.Operation
import Protocol.Package
import Protocol.Types

--------------------------------------------------------------------------------
import Server.Messaging
import Server.RequestId
import Server.Settings
import Server.Storage

--------------------------------------------------------------------------------
data OperationExec =
  OperationExec { _storage :: Storage
                , _chan    :: Chan Msg
                , _ref     :: IORef Requests
                }

--------------------------------------------------------------------------------
type Requests = Map SomeRequestId SomeOp

--------------------------------------------------------------------------------
takeReq :: Typeable a
        => RequestId a
        -> Requests
        -> (Publish Pkg, Operation a, Requests)
takeReq rid m = do
  let sid     = SomeRequestId rid
      Just op = lookup sid m
  case op of
    SomeOp pub o ->
      let Just r = cast o in (pub, r, deleteMap sid m)

--------------------------------------------------------------------------------
insertReq :: Typeable a
          => RequestId a
          -> Publish Pkg
          -> Operation a
          -> Requests
          -> Requests
insertReq rid pub op m =
  insertMap (SomeRequestId rid) (SomeOp pub op) m

--------------------------------------------------------------------------------
data SomeOp = forall a. Typeable a => SomeOp (Publish Pkg) (Operation a)

--------------------------------------------------------------------------------
data OpMsg = OpSend Pkg

--------------------------------------------------------------------------------
data Msg = StorageMsg SomeStorageMsg

--------------------------------------------------------------------------------
newOperationExec :: Settings -> IO OperationExec
newOperationExec setts = do
  chan <- newChan
  let pubStorage = Publish $ \m -> writeChan chan (StorageMsg m)
  s <- newInMemoryStorage setts pubStorage
  r <- newIORef mempty

  let op     = OperationExec s chan r
      action = do
        _ <- forkFinally (worker op) $ \_ -> action
        return ()

  action
  return op

--------------------------------------------------------------------------------
worker :: OperationExec -> IO ()
worker OperationExec{..} = forever (readChan _chan >>= go)
  where
    go :: Msg -> IO ()
    go (StorageMsg (SomeStorageMsg rid msg)) = do
      action <- atomicModifyIORef _ref $ \m ->
        case msg of
          WriteResult w ->
            let (pub, op, m') = takeReq rid m
                tpe =
                  case w of
                    WriteOk num   -> WriteEventsResp num WriteSuccess
                    WriteFailed e ->
                      let flag =
                            case e of
                              WrongExpectedVersion ->
                                WriteWrongExpectedVersion in
                      WriteEventsResp (-1) flag in
            (m', publish pub $ createRespPkg op tpe)

          ReadResult name r -> do
            let (pub, op, m') = takeReq rid m
                tpe =
                  case r of
                    ReadOk num eos xs ->
                      ReadEventsResp name xs ReadSuccess num eos
                    ReadFailed e ->
                      let flag =
                            case e of
                              StreamNotFound -> ReadNoStream in
                      ReadEventsResp name [] flag (-1) True
            (m', publish pub $ createRespPkg op tpe)

      action

--------------------------------------------------------------------------------
executeOperation :: forall a. Typeable a
                 => OperationExec
                 -> Publish Pkg
                 -> Operation a
                 -> IO ()
executeOperation OperationExec{..} pub op@Operation{..} = do
  rid <-
    case operationType of
      WriteEvents name ver xs ->
        appendStream _storage name ver xs

      ReadEvents name batch ->
        readStream _storage name batch

  atomicModifyIORef' _ref $ \m ->
    (insertReq rid pub op m, ())
