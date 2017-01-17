{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : LDE.Internal.Exec
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module LDE.Internal.Exec
  ( Exec
  , newExec
  , execSend
  , execWaitClosed
  ) where

--------------------------------------------------------------------------------
import Control.Exception (AsyncException(..), asyncExceptionFromException)

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Bifoldable
import Protocol.Package

--------------------------------------------------------------------------------
import LDE.Internal.Connection
import LDE.Internal.CyclicQueue
import LDE.Internal.Handler
import LDE.Internal.Settings

--------------------------------------------------------------------------------
data Msg
  = Recv Pkg
  | Stopped SomeException

--------------------------------------------------------------------------------
data Exec =
  Exec { senderSTM :: STM (Msg -> IO ())
       , closedSTM :: STM ()
       }

--------------------------------------------------------------------------------
execSend :: Exec -> Msg -> IO ()
execSend Exec{..} msg = ($ msg) =<< atomically senderSTM

--------------------------------------------------------------------------------
execWaitClosed :: Exec -> IO ()
execWaitClosed Exec{..} = atomically closedSTM

--------------------------------------------------------------------------------
newtype Job = Job { runJob :: IO () }

--------------------------------------------------------------------------------
data Env =
  Env { _setts    :: Settings
      , _connVar  :: TVar InternalConnection
      , _msgQueue :: CyclicQueue Msg
      , _jobQueue :: CyclicQueue Job
      , _pkgQueue :: CyclicQueue Pkg
      }

--------------------------------------------------------------------------------
data Actor =
  Actor { actorThread :: ThreadId
        , actorType   :: ActorType
        }

--------------------------------------------------------------------------------
data ActorType
  = ReceiverActor
  | WriterActor
  | RunnerActor

--------------------------------------------------------------------------------
newEnv :: Settings -> IO Env
newEnv setts =
  Env setts <$> (newConnection setts >>= newTVarIO)
            <*> newCQ
            <*> newCQ
            <*> newCQ

--------------------------------------------------------------------------------
closedMailbox :: Exception e => e -> Msg -> IO ()
closedMailbox e _ = throwIO e

--------------------------------------------------------------------------------
newExec :: Settings -> IO Exec
newExec setts = do
  env      <- newEnv setts
  disposed <- newEmptyTMVarIO
  sendVar  <- newTVarIO (atomically . writeCQ (_msgQueue env))

  let handleOutcome outcome =
        bifor_ outcome (handlesWith handlers) $ \_ ->
          atomically $ putTMVar disposed ()

      handlers =
        [ Handler $ \(e :: ConnectionException) ->
            atomically $ do
              writeTVar sendVar (closedMailbox e)
              putTMVar disposed ()

        , Handler $ \(e :: SomeException) -> do
            conn <- newConnection setts
            atomically $ writeTVar (_connVar env) conn
            _ <- forkFinally (bootstrap env) handleOutcome
            return ()
        ]

  _ <- forkFinally (bootstrap env) handleOutcome
  return $ Exec (readTVar sendVar) $ do
    conn <- readTVar $ _connVar env
    unlessM (connIsClosed conn) retrySTM
    readTMVar disposed

--------------------------------------------------------------------------------
-- | Spawns a new thread worker.
spawn :: Env -> ActorType -> IO Actor
spawn Env{..} tpe = do
    conn <- readTVarIO _connVar

    let action =
          case tpe of
            ReceiverActor -> receiver _msgQueue conn
            RunnerActor   -> runner _jobQueue
            WriterActor   -> writer _pkgQueue conn

    tid <- forkFinally action $ \outcome ->
      case outcome of
        Left e ->
          case asyncExceptionFromException e of
            Just ThreadKilled -> return ()
            _ -> atomically $ writeCQ _msgQueue (Stopped e)

    return $ Actor tid tpe

--------------------------------------------------------------------------------
receiver :: CyclicQueue Msg -> InternalConnection -> IO ()
receiver msgQueue c = forever $ do
  pkg <- connRecv c
  atomically $ writeCQ msgQueue (Recv pkg)

--------------------------------------------------------------------------------
runner :: CyclicQueue Job -> IO ()
runner jobQueue = forever $ do
  j <- atomically $ readCQ jobQueue
  runJob j

--------------------------------------------------------------------------------
writer :: CyclicQueue Pkg-> InternalConnection -> IO ()
writer pkgQueue conn = forever $ do
  pkg <- atomically $ readCQ pkgQueue
  connSend conn pkg

--------------------------------------------------------------------------------
bootstrap :: Env -> IO ()
bootstrap _ = return ()
