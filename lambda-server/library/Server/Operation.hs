{-# LANGUAGE RecordWildCards #-}
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
import ClassyPrelude
import Protocol.Message
import Protocol.Operation
import Protocol.Package
import Protocol.Types

--------------------------------------------------------------------------------
import Server.Settings
import Server.Storage

--------------------------------------------------------------------------------
data OperationExec =
  OperationExec { storage :: Storage }

--------------------------------------------------------------------------------
data OpMsg
  = OpSend Pkg
  | OpNoop

--------------------------------------------------------------------------------
newOperationExec :: Settings -> IO OperationExec
newOperationExec setts =
  OperationExec <$> newInMemoryStorage setts

--------------------------------------------------------------------------------
executeOperation :: OperationExec -> Operation -> IO OpMsg
executeOperation OperationExec{..} Operation{..} =
  case operationType of
    WriteEvents name ver xs -> do
      outcome <- appendStream storage name ver xs

      let respType =
            case outcome of
              WriteOk num ->
                WriteEventsResp num WriteSuccess
              WriteFailed e ->
                let flag =
                      case e of
                        WrongExpectedVersion -> WriteWrongExpectedVersion in
                WriteEventsResp (-1) flag

          resp = Response operationId respType

      return $ OpSend $ createRespPkg resp

    ReadEvents name batch -> do
      outcome <- readStream storage name batch

      let respType =
            case outcome of
              ReadOk num eos xs ->
                ReadEventsResp name xs ReadSuccess num eos
              ReadFailed e ->
                let flag =
                      case e of
                        StreamNotFound -> ReadNoStream in
                ReadEventsResp name [] flag (-1) True

          resp = Response operationId respType

      return $ OpSend $ createRespPkg resp
