--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Client.Connection
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Lambda.Client.Connection where

--------------------------------------------------------------------------------
import Lambda.Bus
import Lambda.Prelude
import Network.Connection

--------------------------------------------------------------------------------
import Lambda.Client.Settings
import Lambda.Client.TcpConnection

--------------------------------------------------------------------------------
data Internal =
  Internal { _internal :: !Int }

--------------------------------------------------------------------------------
new :: Lambda Settings ()
new = return ()


