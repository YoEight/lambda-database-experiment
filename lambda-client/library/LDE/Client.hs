{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : LDE.Client
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module LDE.Client
  ( Client
  , newClient
  , writeEvents
  ) where

--------------------------------------------------------------------------------
import Data.List.NonEmpty

--------------------------------------------------------------------------------
import ClassyPrelude
import Protocol.Operation
import Protocol.Types

--------------------------------------------------------------------------------
import LDE.Internal.Command
import LDE.Internal.Exec
import LDE.Internal.Settings

--------------------------------------------------------------------------------
data Client = Client { _exec :: Exec }

--------------------------------------------------------------------------------
newClient :: ConnectionType -> IO Client
newClient tpe = Client <$> newExec (defaultSettings tpe)

--------------------------------------------------------------------------------
data WriteResult
  = WriteOk EventNumber
  | WriteFailure WriteFailure

--------------------------------------------------------------------------------
data WriteFailure = WrongExpectedVersion deriving Show

--------------------------------------------------------------------------------
instance Exception WriteFailure

--------------------------------------------------------------------------------
writeEvents :: Client
            -> StreamName
            -> ExpectedVersion
            -> NonEmpty Event
            -> IO (Async WriteResult)
writeEvents Client{..} n ver evts = do
  (a, k) <- newPromise

  let cmd = Command { commandReq = WriteEvents n ver evts
                    , commandCb  = \(WriteEventsResp num flag) -> do
                        case flag of
                          WriteSuccess ->
                            k (WriteOk num)
                          WriteWrongExpectedVersion ->
                            k (WriteFailure WrongExpectedVersion)
                    }

  execSubmit _exec cmd
  return a

--------------------------------------------------------------------------------
newPromise :: IO (Async a, a -> IO ())
newPromise = do
  mvar <- newEmptyMVar
  a <- async $ readMVar mvar
  return (a, putMVar mvar)
