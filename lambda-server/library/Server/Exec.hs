--------------------------------------------------------------------------------
-- |
-- Module : Server.Exec
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Exec (exec) where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
import Server.Connection
import Server.Settings

--------------------------------------------------------------------------------
exec :: Settings -> IO ()
exec setts = do
  conn <- newServerConnection $ connectionSettings setts
  forever $ do
    client <- awaitClientConnection conn
    say "New connection"
    fork $ exchange client

--------------------------------------------------------------------------------
exchange :: ClientConnection -> IO ()
exchange client = return ()
