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
import Protocol.Operation
import Protocol.Package

--------------------------------------------------------------------------------
import Server.Settings

--------------------------------------------------------------------------------
data OperationExec =
  OperationExec

--------------------------------------------------------------------------------
data OpMsg
  = OpSend Pkg
  | OpNoop

--------------------------------------------------------------------------------
newOperationExec :: Settings -> IO OperationExec
newOperationExec _ = return OperationExec

--------------------------------------------------------------------------------
executeOperation :: OperationExec -> Operation -> IO OpMsg
executeOperation _ _ = return OpNoop
