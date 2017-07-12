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
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Monad.Logger hiding (logDebug, logInfo, logWarn, logError)
import Control.Monad.Logger.CallStack
