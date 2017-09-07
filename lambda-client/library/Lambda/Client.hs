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
import Lambda.Bus
import Lambda.Prelude

--------------------------------------------------------------------------------
import qualified Lambda.Client.Connection as Connection
import           Lambda.Client.Settings
import           Lambda.Client.TcpConnection

--------------------------------------------------------------------------------
data Client =
  Client
  { _settings :: Settings
  , _mainBus  :: Bus Settings
  }

--------------------------------------------------------------------------------
newClient :: Settings -> IO Client
newClient setts = lambdaMain setts $ do
  mainBus <- newBus
  builder <- connectionBuilder

  configure mainBus (Connection.app builder)

  let client = Client setts mainBus
  return client

--------------------------------------------------------------------------------
newClientWithDefault :: IO Client
newClientWithDefault = newClient defaultSettings
