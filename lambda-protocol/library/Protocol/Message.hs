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
import ClassyPrelude

--------------------------------------------------------------------------------
import qualified Protocol.Message.WriteEvents as WriteEvents
import qualified Protocol.Message.ReadEvents as ReadEvents
import           Protocol.Operation
import           Protocol.Package

--------------------------------------------------------------------------------
createPkg :: Operation -> Pkg
createPkg Operation{..} =
  case operationType of
    WriteEvents name ver xs ->
      WriteEvents.createPkg operationId name ver xs
    ReadEvents name b ->
      ReadEvents.createPkg operationId name b

--------------------------------------------------------------------------------
createRespPkg :: Response -> Pkg
createRespPkg Response{..} =
  case responseType of
    WriteEventsResp num flag ->
      WriteEvents.createRespPkg responseId num flag
    ReadEventsResp name xs flag num eos ->
      ReadEvents.createRespPkg responseId name xs flag num eos

--------------------------------------------------------------------------------
parseOp :: Pkg -> Maybe Operation
parseOp pkg =
  WriteEvents.parseOp pkg <|> ReadEvents.parseOp pkg

--------------------------------------------------------------------------------
parseResp :: Pkg -> Maybe Response
parseResp pkg =
  WriteEvents.parseResp pkg <|> ReadEvents.parseResp pkg
