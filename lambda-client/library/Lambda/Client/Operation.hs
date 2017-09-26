--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Client.Operation
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Lambda.Client.Operation where

--------------------------------------------------------------------------------
import Numeric.Natural

--------------------------------------------------------------------------------
import Lambda.Bus
import Lambda.Prelude
import Lambda.Prelude.Stopwatch
import Protocol.Message
import Protocol.Operation
import Protocol.Package

--------------------------------------------------------------------------------
import Lambda.Client.Messages
import Lambda.Client.Settings
import Lambda.Client.TcpConnection

--------------------------------------------------------------------------------
data Meta =
  Meta { attempts :: !Natural
       , started  :: !NominalDiffTime
       }

--------------------------------------------------------------------------------
newMeta :: Stopwatch -> React Settings Meta
newMeta s = Meta 0 <$> stopwatchElapsed s

--------------------------------------------------------------------------------
data Pending where
  Pending :: Meta
          -> Operation a
          -> (Either String a -> IO ())
          -> Pending

--------------------------------------------------------------------------------
data Awaiting where
  Awaiting :: Request a
           -> (Either String a -> IO ())
           -> Awaiting

--------------------------------------------------------------------------------
data Manager =
  Manager { connRef   :: ConnectionRef
          , pendings  :: IORef (HashMap PkgId Pending)
          , awaitings :: IORef (Seq Awaiting)
          , stopwatch :: Stopwatch
          }

--------------------------------------------------------------------------------
new :: ConnectionRef -> Configure Settings Manager
new ref =
  Manager ref <$> newIORef mempty
              <*> newIORef mempty
              <*> newStopwatch

--------------------------------------------------------------------------------
submit :: Manager -> NewRequest -> React Settings ()
submit Manager{..} (NewRequest req respond) =
  maybeConnection connRef >>= \case
    Just conn ->
      do pkgId <- freshPkgId
         meta  <- newMeta stopwatch
         let op      = Operation pkgId req
             pending = Pending meta op respond
             pkg     = createPkg op

         modifyIORef' pendings (insertMap pkgId pending)
         enqueuePkg conn pkg
         logDebug "New request package enqueued."

    Nothing ->
      do logDebug "Connection not available. Add request to the waiting queue."
         let awaiting = Awaiting req respond
         modifyIORef' awaitings (`snoc` awaiting)

--------------------------------------------------------------------------------
arrived :: Manager -> Pkg -> React Settings ()
arrived Manager{..} pkg@Pkg{..} = do
  reg <- readIORef pendings
  case lookup pkgId reg of
    Nothing -> logWarn [i|Unknown request #{pkgId} response. Discarded.|]
    Just (Pending _ op@(Operation _ req) callback) ->
      do case parseResp pkg req of
           Nothing ->
             do logError [i|Unexpected request response on #{pkgId}. Discarded|]
                liftIO $ callback (Left "Unexpected request")
           Just resp -> liftIO $ callback (Right $ responseType resp)

         writeIORef pendings (deleteMap pkgId reg)

--------------------------------------------------------------------------------
-- TODO - Implement pending request checking so we can detect which operation
--        has timeout.
tick :: Manager -> React Settings ()
tick self@Manager{..} = do
  logDebug "Enter tick..."
  as <- atomicModifyIORef' awaitings $ \cur -> (mempty, cur)
  traverse_ submitting as
  logDebug "Leave tick."
  where
    submitting (Awaiting req respond) =
      submit self (NewRequest req respond)

