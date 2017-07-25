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
import Lambda.Client.Prelude

--------------------------------------------------------------------------------
data Settings =
  Settings
  { connectionType :: !ConnectionType

  }

--------------------------------------------------------------------------------
defaultSettings :: Settings
defaultSettings =
  Settings { connectionType = Static "localhost" 1113 }

--------------------------------------------------------------------------------
data ConnectionType = Static String Int

--------------------------------------------------------------------------------
