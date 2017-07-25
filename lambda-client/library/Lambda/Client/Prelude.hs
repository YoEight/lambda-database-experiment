--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Client.Prelude
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Client.Prelude
  ( module ClassyPrelude
  , module Control.Monad.Logger
  , module Control.Monad.Logger.CallStack
  , module Data.String.Interpolate.IsString
  , module Data.Time
  , module Control.Monad.Fix
  , UUID
  , freshUUID
  , s2mcs
  ) where

--------------------------------------------------------------------------------
import Control.Monad.Fix

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.Logger hiding (logDebug, logInfo, logWarn, logError)
import Control.Monad.Logger.CallStack
import Data.UUID
import Data.UUID.V4
import Data.String.Interpolate.IsString
import Data.Time hiding (getCurrentTime)

--------------------------------------------------------------------------------
freshUUID :: MonadIO m => m UUID
freshUUID = liftIO nextRandom

--------------------------------------------------------------------------------
s2mcs :: Num a => a
s2mcs = 10^(6 :: Int)
