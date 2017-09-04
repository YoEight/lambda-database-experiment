{-# LANGUAGE GADTs           #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : Protocol.Message
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Protocol.Message
  ( createPkg
  , createRespPkg
  , parseOp
  , parseResp
  ) where

--------------------------------------------------------------------------------
import Lambda.Prelude

--------------------------------------------------------------------------------
import qualified Protocol.Message.WriteEvents as WriteEvents
import qualified Protocol.Message.ReadEvents as ReadEvents
import           Protocol.Operation
import           Protocol.Package

--------------------------------------------------------------------------------
createPkg :: forall a. Operation a -> Pkg
createPkg Operation{..} =
  case operationType of
    WriteEvents name ver xs ->
      WriteEvents.createPkg operationId name ver xs
    ReadEvents name b ->
      ReadEvents.createPkg operationId name b

--------------------------------------------------------------------------------
createRespPkg :: forall a. Operation a -> a -> Pkg
createRespPkg Operation{..} = go operationType
  where
    go :: forall a. Request a -> a -> Pkg
    go WriteEvents{} (WriteEventsResp num flag) =
      WriteEvents.createRespPkg operationId num flag
    go ReadEvents{} (ReadEventsResp name xs flag num eos) =
      ReadEvents.createRespPkg operationId name xs flag num eos

--------------------------------------------------------------------------------
parseOp :: Pkg -> Maybe SomeOperation
parseOp pkg =
  fmap SomeOperation (WriteEvents.parseOp pkg) <|>
  fmap SomeOperation (ReadEvents.parseOp pkg)

--------------------------------------------------------------------------------
parseResp :: forall a. Pkg -> Request a ->  Maybe (Response a)
parseResp pkg = go
  where
    go :: forall a. Request a -> Maybe (Response a)
    go WriteEvents{} = WriteEvents.parseResp pkg
    go ReadEvents{}  = ReadEvents.parseResp pkg
