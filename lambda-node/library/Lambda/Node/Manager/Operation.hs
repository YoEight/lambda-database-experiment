--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Node.Manager.Operation
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Manager.Operation
    ( Manager
    , Resp(..)
    , new
    , push
    ) where

--------------------------------------------------------------------------------
import Lambda.Bus
import Lambda.Prelude
import Protocol.Operation
import Protocol.Types

--------------------------------------------------------------------------------
import qualified Lambda.Node.Index as Index
import           Lambda.Node.Settings

--------------------------------------------------------------------------------
data Req where
  Req :: SomeBus Settings -> Operation a -> Req

--------------------------------------------------------------------------------
data Resp where
  Resp :: Operation a -> a -> Resp

--------------------------------------------------------------------------------
data Manager = Manager { _bus   :: Bus Settings
                       , _index :: Index.Indexer
                       }

--------------------------------------------------------------------------------
new :: React Settings Manager
new = reactLambda $ do
  bus <- newBus
  ind <- Index.newIndexer

  let self = Manager bus ind

  configure bus $
    do subscribe (onReq self)

  pure self

--------------------------------------------------------------------------------
onReq :: Manager -> Req -> React Settings ()
onReq self (Req sender op@(Operation _ req)) =
  case req of
    WriteEvents name _ evts -> do
      num <- reactLambda $ Index.indexEvents (_index self) name (toList evts)

      let resp = WriteEventsResp num WriteSuccess

      sendTo sender (Resp op resp)
    ReadEvents name _ -> do
      res <- reactLambda $ Index.readStreamEvents (_index self) name

      let evts = foldMap toList res
          resp = ReadEventsResp name evts ReadSuccess (-1) True

      sendTo sender (Resp op resp)

--------------------------------------------------------------------------------
push :: Manager -> Operation a -> React Settings ()
push self op = do
  sender <- reactBus
  sendTo (_bus self) (Req sender op)
