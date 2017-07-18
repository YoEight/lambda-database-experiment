--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Node.Prelude
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Prelude
  ( module ClassyPrelude
  , module Control.Monad.Logger
  , module Control.Monad.Logger.CallStack
  , module Data.String.Interpolate.IsString
  , module Data.Time
  , module System.Clock
  , module Control.Monad.Fix
  , UUID
  , clockTime
  , freshUUID
  , s2ns
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
import Data.Time
import System.Clock

--------------------------------------------------------------------------------
clockTime :: MonadIO m => m TimeSpec
clockTime = liftIO $ getTime Monotonic

--------------------------------------------------------------------------------
freshUUID :: MonadIO m => m UUID
freshUUID = liftIO nextRandom

--------------------------------------------------------------------------------
s2ns :: Num a => a
s2ns = 10^9

--------------------------------------------------------------------------------
s2mcs :: Num a => a
s2mcs = 10^6