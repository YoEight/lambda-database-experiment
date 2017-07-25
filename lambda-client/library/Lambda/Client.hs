--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Client
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Client
  ( Client
  , newClient
  , newClientWithDefault
  ) where

--------------------------------------------------------------------------------
import Lambda.Client.Prelude
import Lambda.Client.Settings

--------------------------------------------------------------------------------
data Client =
  Client
  { _settings :: Settings }

--------------------------------------------------------------------------------
newClient :: Settings -> IO Client
newClient = return . Client

--------------------------------------------------------------------------------
newClientWithDefault :: IO Client
newClientWithDefault = newClient defaultSettings
