--------------------------------------------------------------------------------
-- |
-- Module : Server.Settings
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Settings where

--------------------------------------------------------------------------------
import Server.Connection
import Server.Timer

--------------------------------------------------------------------------------
data Settings =
  Settings { connectionSettings :: ConnectionSettings
           , heartbeatInterval  :: Duration
           , heartbeatTimeout   :: Duration
           }
