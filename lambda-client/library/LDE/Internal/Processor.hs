{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module : LDE.Internal.Processor
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module LDE.Internal.Processor
  ( Proc
  , ProcOutcome(..)
  , submitCmd
  , submitPkg
  , newProc
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Protocol.Message
import Protocol.Package
import Protocol.Operation

--------------------------------------------------------------------------------
import LDE.Internal.Command
import LDE.Internal.Publish

--------------------------------------------------------------------------------
data ProcOutcome
  = SendPkg Pkg
  | Run (IO ())

--------------------------------------------------------------------------------
type Commands = Map PkgId SomeCommand

--------------------------------------------------------------------------------
data Proc =
  Proc { _pub  :: Publish ProcOutcome
       , _cmds :: IORef Commands
       }

--------------------------------------------------------------------------------
newProc :: Publish ProcOutcome -> IO Proc
newProc pub = Proc pub <$> newIORef mempty

--------------------------------------------------------------------------------
submitCmd :: Proc -> SomeCommand -> IO ()
submitCmd Proc{..} cmd@(SomeCommand c) = do
  pid <- freshPkgId
  let op  = Operation pid (commandReq c)
      pkg = createPkg op

  atomicModifyIORef' _cmds $ \m ->
    let m' = insertMap pid cmd m in (m', ())

  publish _pub (SendPkg pkg)


--------------------------------------------------------------------------------
submitPkg :: Proc -> Pkg -> IO ()
submitPkg Proc{..} pkg@Pkg{..} =
  case pkgCmd of
    0x01 -> publish _pub (SendPkg $ heartbeatResponse pkgId)
    _    -> do
      cmds <- readIORef _cmds

      let action = do
            SomeCommand cmd <- lookup pkgId cmds
            resp            <- responseType <$> parseResp pkg (commandReq cmd)

            return (commandCb cmd resp)

      for_ action $ \job -> do
        atomicModifyIORef' _cmds $ \m ->
          let m' = deleteMap pkgId m in (m', ())

        publish _pub (Run job)
