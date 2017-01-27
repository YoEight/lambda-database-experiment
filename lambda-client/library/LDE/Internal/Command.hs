{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : LDE.Internal.Command
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module LDE.Internal.Command where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Protocol.Operation

--------------------------------------------------------------------------------
data Command a =
  Command { commandReq :: Request a
          , commandCb  :: a -> IO ()
          }

--------------------------------------------------------------------------------
data SomeCommand = forall a. SomeCommand (Command a)
