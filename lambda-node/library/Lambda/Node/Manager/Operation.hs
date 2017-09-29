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
import qualified Data.Aeson as Aeson
import           Data.Aeson (object, (.=))
import           Lambda.Bus
import           Lambda.Prelude
import           Protocol.Operation
import           Protocol.Types

--------------------------------------------------------------------------------
import Lambda.Node.Settings

--------------------------------------------------------------------------------
data Req where
  Req :: SomeBus Settings -> Operation a -> Req

--------------------------------------------------------------------------------
data Resp where
  Resp :: Operation a -> a -> Resp

--------------------------------------------------------------------------------
data Manager = Manager { _bus :: Bus Settings }

--------------------------------------------------------------------------------
new :: React Settings Manager
new = reactLambda $ do
  bus <- newBus
  configure bus app
  pure $ Manager bus

--------------------------------------------------------------------------------
app :: Configure Settings ()
app = subscribe onReq

--------------------------------------------------------------------------------
onReq :: Req -> React Settings ()
onReq (Req sender op@(Operation _ req)) =
  case req of
    WriteEvents{} ->
      let resp = WriteEventsResp 1 WriteSuccess
       in sendTo sender (Resp op resp)
    ReadEvents name _ -> do
      eid <- freshId
      let payload = object [ "IsHaskellTheBest" .= True ]
          evt     = Event { eventType = "lde-mockup"
                          , eventId   = eid
                          , eventPayload = Data $ toStrict $ Aeson.encode $ payload
                          , eventMetadata = Nothing
                          }
          saved = SavedEvent 1 evt
          resp  = ReadEventsResp name [saved] ReadSuccess (-1) True

      sendTo sender (Resp op resp)

--------------------------------------------------------------------------------
push :: Manager -> Operation a -> React Settings ()
push self op = do
  sender <- reactBus
  sendTo (_bus self) (Req sender op)
