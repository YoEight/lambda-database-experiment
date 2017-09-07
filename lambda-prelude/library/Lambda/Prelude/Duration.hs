--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Prelude.Duration
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Lambda.Prelude.Duration where

--------------------------------------------------------------------------------
import Lambda.Prelude

--------------------------------------------------------------------------------
newtype Duration = Duration Int64 deriving Show

--------------------------------------------------------------------------------
msDuration :: Int64 -> Duration
msDuration = Duration . (1000 *)

--------------------------------------------------------------------------------
secsDuration :: Int64 -> Duration
secsDuration = msDuration . (1000 *)
