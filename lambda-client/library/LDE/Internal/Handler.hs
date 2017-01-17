{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : LDE.Internal.Handler
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module LDE.Internal.Handler where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
handlesWith :: [Handler IO a] -> SomeException -> IO a
handlesWith = go
  where
    go [] e             = throwIO e
    go (Handler k:ks) e =
      case fromException e of
        Just t -> k t
        _      -> go ks e
