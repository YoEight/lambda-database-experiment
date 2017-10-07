--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Client.Operation
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Operation manager. It prepares requests from the user and handle responses
-- from the database server. It also manages operations timeout.
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
-- | Meta-information related to operation transaction.
data Meta =
  Meta { attempts :: !Natural
         -- ^ Number of time this operation has been tried.
       , correlation :: !PkgId
         -- ^ Id of the transaction.
       , started :: !NominalDiffTime
         -- ^ Since when this operation has been emitted.
       }

--------------------------------------------------------------------------------
-- | Creates a new 'Meta'.
newMeta :: Stopwatch -> React Settings Meta
newMeta s = Meta 0 <$> freshPkgId
                   <*> stopwatchElapsed s

--------------------------------------------------------------------------------
-- | Represents an ongoing transaction.
data Pending where
  Pending :: Meta
          -> Request a
          -> (Either String a -> IO ()) -- The callback to execute once we get a response.
          -> Pending

--------------------------------------------------------------------------------
-- | Represents a request put on hold because at the moment it was submitted,
--   an open 'TcpConnection' wasn't available. Those requests will be retried
--   once the connection manager calls 'tick'.
data Awaiting where
  Awaiting :: Request a
           -> (Either String a -> IO ())
           -> Awaiting

--------------------------------------------------------------------------------
-- | Operation manager reference.
data Manager =
  Manager { connRef :: ConnectionRef
            -- ^ Allows us to know if a 'TcpConnection' is available.
          , pendings  :: IORef (HashMap PkgId Pending)
          , awaitings :: IORef (Seq Awaiting)
          , stopwatch :: Stopwatch
          }

--------------------------------------------------------------------------------
-- | Creates an operation manager instance.
new :: ConnectionRef -> Configure Settings Manager
new ref =
  Manager ref <$> newIORef mempty
              <*> newIORef mempty
              <*> newStopwatch

--------------------------------------------------------------------------------
-- | Submits a new operation request.
submit :: Manager -> NewRequest -> React Settings ()
submit Manager{..} (NewRequest req callback) =
  maybeConnection connRef >>= \case
    Just conn ->
      do meta  <- newMeta stopwatch
         let op      = Operation (correlation meta) req
             pending = Pending meta req callback
             pkg     = createPkg op

         modifyIORef' pendings (insertMap (correlation meta) pending)
         enqueuePkg conn pkg

    Nothing ->
      let awaiting = Awaiting req callback
       in modifyIORef' awaitings (`snoc` awaiting)

--------------------------------------------------------------------------------
-- | Updates operation manager state by submitting a incoming 'Pkg'.
arrived :: Manager -> Pkg -> React Settings ()
arrived Manager{..} pkg@Pkg{..} = do
  reg <- readIORef pendings
  case lookup pkgId reg of
    Nothing -> logWarn [i|Unknown request #{pkgId} response. Discarded.|]
    Just (Pending _ req callback) ->
      do case parseResp pkg req of
           Nothing ->
             do logError [i|Unexpected request response on #{pkgId}. Discarded|]
                liftIO $ callback (Left "Unexpected request")
           Just resp -> liftIO $ callback (Right $ responseType resp)

         writeIORef pendings (deleteMap pkgId reg)

--------------------------------------------------------------------------------
-- | Performs operation manager internal bookkeepping like keeping track of
--   timeout operations or retrying awaited requests.
-- TODO - Implement pending request checking so we can detect which operation
--        has timeout.
tick :: Manager -> React Settings ()
tick self@Manager{..} = do
  logDebug "Enter tick..."
  as <- atomicModifyIORef' awaitings $ \cur -> (mempty, cur)
  traverse_ submitting as
  logDebug "Leave tick."
  where
    submitting (Awaiting req callback) =
      submit self (NewRequest req callback)

