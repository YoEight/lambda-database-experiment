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

--------------------------------------------------------------------------------
exec :: ConnectionSettings -> IO ()
exec setts = do
  conn <- newServerConnection setts
  forever $ do
    client <- awaitClientConnection conn
    say "New connection"
    say $ clientId client
    fork $ exchange client

--------------------------------------------------------------------------------
exchange :: ClientConnection -> IO ()
exchange client = return ()
