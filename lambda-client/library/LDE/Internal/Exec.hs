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
import Control.Monad.Fix

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Bifoldable
import Protocol.Package

--------------------------------------------------------------------------------
import LDE.Internal.Command
import LDE.Internal.Connection
import LDE.Internal.CyclicQueue
import LDE.Internal.Handler
import LDE.Internal.Processor
import LDE.Internal.Publish
import LDE.Internal.Settings

--------------------------------------------------------------------------------
data Msg
  = Recv Pkg
  | Stopped SomeException
  | Submit SomeCommand

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
      , _stateRef :: IORef ManagerState
      , _proc     :: Proc
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
newEnv setts = mfix $ \env ->
  Env setts <$> (newConnection setts >>= newTVarIO)
            <*> newIORef initManagerState
            <*> newProc (pub env)
            <*> newCQ
            <*> newCQ
            <*> newCQ

  where
    pub Env{..} = Publish $ \outcome -> atomically $
      case outcome of
        SendPkg pkg -> writeCQ _pkgQueue pkg
        Run action  -> writeCQ _jobQueue (Job action)

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
spawn :: Env -> ActorType -> IO ()
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

  atomicModifyIORef' _stateRef $ \s ->
    let act = Actor tid tpe
        s'  =
          case tpe of
            ReceiverActor -> s { receiverAct = Just act }
            RunnerActor   -> s { runnerAct   = Just act }
            WriterActor   -> s { writerAct   = Just act } in
    (s', ())

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
writer :: CyclicQueue Pkg -> InternalConnection -> IO ()
writer pkgQueue conn = forever $ do
  pkg <- atomically $ readCQ pkgQueue
  connSend conn pkg

--------------------------------------------------------------------------------
data ManagerState =
  ManagerState { receiverAct :: Maybe Actor
               , writerAct   :: Maybe Actor
               , runnerAct   :: Maybe Actor
               }

--------------------------------------------------------------------------------
initManagerState :: ManagerState
initManagerState =
  ManagerState { receiverAct = Nothing
               , writerAct   = Nothing
               , runnerAct   = Nothing
               }

--------------------------------------------------------------------------------
bootstrap :: Env -> IO ()
bootstrap env = do
  spawn env ReceiverActor
  spawn env RunnerActor
  spawn env WriterActor

  cruising env

--------------------------------------------------------------------------------
cruising :: Env -> IO ()
cruising env@Env{..} = forever $ do
  msg <- atomically $ readCQ _msgQueue

  case msg of
    Stopped e  -> throwIO e
    Recv pkg   -> submitPkg _proc pkg
    Submit cmd -> submitCmd _proc cmd
