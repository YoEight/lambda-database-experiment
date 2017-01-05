--------------------------------------------------------------------------------
-- |
-- Module : Protocol.Operation
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Protocol.Operation where

--------------------------------------------------------------------------------
import Data.List.NonEmpty

--------------------------------------------------------------------------------
import Protocol.Types

--------------------------------------------------------------------------------
data Operation
  = WriteEvents StreamName ExpectedVersion (NonEmpty Event)
  | ReadEvents StreamName Batch
