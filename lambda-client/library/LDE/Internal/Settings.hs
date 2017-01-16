--------------------------------------------------------------------------------
-- |
-- Module : LDE.Internal.Settings
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module LDE.Internal.Settings where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
-- | Gathers every connection type handled by the client.
data ConnectionType
  = Static String Int
    -- ^ HostName and Port.

--------------------------------------------------------------------------------
-- | Represents reconnection strategy.
data Retry
  = AtMost Int
  | KeepRetrying

--------------------------------------------------------------------------------
-- | Indicates how many times we should try to reconnect to the server. A value
--   less than or equal to 0 means no retry.
atMost :: Int -> Retry
atMost = AtMost

--------------------------------------------------------------------------------
-- | Indicates we should try to reconnect to the server until the end of the
--   Universe.
keepRetrying :: Retry
keepRetrying = KeepRetrying

--------------------------------------------------------------------------------
-- | Global 'Connection' settings
data Settings =
  Settings
  { s_retry                :: Retry
  , s_reconnect_delay_secs :: Int -- ^ In seconds
  , s_connection           :: ConnectionType
  }

--------------------------------------------------------------------------------
-- | Default global settings.
--   s_retry                = 'atMost' 3
--   s_reconnect_delay_secs = 3
defaultSettings :: ConnectionType -> Settings
defaultSettings tpe =
  Settings
  { s_retry                = atMost 3
  , s_reconnect_delay_secs = 3
  , s_connection           = tpe
  }
