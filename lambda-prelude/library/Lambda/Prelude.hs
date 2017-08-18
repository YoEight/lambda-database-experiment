--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Prelude
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Prelude
  ( module ClassyPrelude
  , module Control.Monad.Logger
  , module Control.Monad.Logger.CallStack
  , module Data.String.Interpolate.IsString
  , module System.Clock
  , module Control.Monad.Fix
  , UUID
  , NominalDiffTime
  , clockTime
  , freshUUID
  , s2ns
  , s2mcs
  ) where

--------------------------------------------------------------------------------
import Control.Monad.Fix

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.Logger hiding (logDebug, logInfo, logWarn, logError, logOther, logWarnSH, logOtherSH, logDebugSH, logInfoSH, logErrorSH)
import Control.Monad.Logger.CallStack
import Data.Time (NominalDiffTime)
import Data.UUID
import Data.UUID.V4
import Data.String.Interpolate.IsString
import System.Clock

--------------------------------------------------------------------------------
clockTime :: MonadIO m => m TimeSpec
clockTime = liftIO $ getTime Monotonic

--------------------------------------------------------------------------------
freshUUID :: MonadIO m => m UUID
freshUUID = liftIO nextRandom

--------------------------------------------------------------------------------
s2ns :: Num a => a
s2ns = 10^(9 :: Int)

--------------------------------------------------------------------------------
s2mcs :: Num a => a
s2mcs = 10^(6 :: Int)
