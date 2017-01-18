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
  , Decision(..)
  , submitCmd
  , submitPkg
  , newProc
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Protocol.Package

--------------------------------------------------------------------------------
import LDE.Internal.Command

--------------------------------------------------------------------------------
data Decision
  = SendPkg Pkg
  | Run (IO ())
  | Noop

--------------------------------------------------------------------------------
data Proc = Proc

--------------------------------------------------------------------------------
newProc :: IO Proc
newProc = return Proc

--------------------------------------------------------------------------------
submitCmd :: Proc -> Command -> IO Decision
submitCmd = undefined

--------------------------------------------------------------------------------
submitPkg :: Proc -> Pkg -> IO Decision
submitPkg _ pkg =
  case pkgCmd pkg of
    0x01 -> return $ SendPkg $ heartbeatResponse (pkgId pkg)
    _    -> return Noop
