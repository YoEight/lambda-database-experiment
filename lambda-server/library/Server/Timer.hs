--------------------------------------------------------------------------------
-- |
-- Module : Server.Timer
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Server.Timer where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
newtype Duration = Duration Int64 deriving Show

--------------------------------------------------------------------------------
msDuration :: Int64 -> Duration
msDuration = Duration . (1000 *)

--------------------------------------------------------------------------------
secsDuration :: Int64 -> Duration
secsDuration = msDuration . (1000 *)

--------------------------------------------------------------------------------
delayed :: Duration -> IO () -> IO ()
delayed (Duration timespan) action = () <$ fork (go timespan)
  where
    go i = do
      when (i > 0) $ do
        let wait = min i (fromIntegral (maxBound :: Int))
        threadDelay $ fromIntegral wait
        go (timespan - wait)

      action
