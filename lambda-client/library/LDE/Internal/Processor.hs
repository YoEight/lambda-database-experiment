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
import Protocol.Package

--------------------------------------------------------------------------------
import LDE.Internal.Command
import LDE.Internal.Publish

--------------------------------------------------------------------------------
data ProcOutcome
  = SendPkg Pkg
  | Run (IO ())

--------------------------------------------------------------------------------
data Proc =
  Proc { _pub :: Publish ProcOutcome }

--------------------------------------------------------------------------------
newProc :: Publish ProcOutcome -> IO Proc
newProc pub = return $ Proc pub

--------------------------------------------------------------------------------
submitCmd :: Proc -> Command -> IO ()
submitCmd = undefined

--------------------------------------------------------------------------------
submitPkg :: Proc -> Pkg -> IO ()
submitPkg Proc{..} pkg =
  case pkgCmd pkg of
    0x01 -> publish _pub (SendPkg $ heartbeatResponse (pkgId pkg))
    _    -> return ()
