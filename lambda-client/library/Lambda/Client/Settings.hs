--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Client.Settings
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Client.Settings where

--------------------------------------------------------------------------------
import Lambda.Prelude

--------------------------------------------------------------------------------
-- | Client settings.
data Settings =
  Settings
  { connectionType :: !ConnectionType
    -- ^ Connection type.
  , connectionTimeout :: !NominalDiffTime
    -- ^ How long a connection much take before being considered as timeout.
  }

--------------------------------------------------------------------------------
-- * 'connectionType' = 'Static' /"localhost"/ /1113/.
-- * 'connectionTimeout' = /3/ seconds.
defaultSettings :: Settings
defaultSettings =
  Settings { connectionType = Static "localhost" 1113
           , connectionTimeout = 3 -- secs.
           }

--------------------------------------------------------------------------------
-- | Types of connection supported.
data ConnectionType = Static String Int

--------------------------------------------------------------------------------
