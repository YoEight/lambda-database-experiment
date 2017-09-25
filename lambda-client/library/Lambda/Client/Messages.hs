--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Client.Messages
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Lambda.Client.Messages where

--------------------------------------------------------------------------------
import Lambda.Prelude
import Protocol.Operation

--------------------------------------------------------------------------------
data NewRequest where
  NewRequest :: Request a -> (Either String a -> IO ()) -> NewRequest

