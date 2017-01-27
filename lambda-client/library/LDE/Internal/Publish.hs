--------------------------------------------------------------------------------
-- |
-- Module : LDE.Internal.Publish
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module LDE.Internal.Publish where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
newtype Publish a = Publish { publish :: a -> IO () }
