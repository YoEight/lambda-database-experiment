{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
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
  , module Data.String.Interpolate.IsString
  , module System.Clock
  , module Control.Monad.Fix
  , module Lambda.Logger
  -- * Lambda
  , Lambda
  , lambdaMain
  -- * Misc
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
import Control.Monad.Reader
import Data.Time (NominalDiffTime)
import Data.UUID
import Data.UUID.V4
import Data.String.Interpolate.IsString
import Lambda.Logger
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

--------------------------------------------------------------------------------
data Env app =
  Env { _settings  :: !(Settings app)
      , _loggerRef :: !LoggerRef
      }

--------------------------------------------------------------------------------
instance Functor Env where
  fmap f e = e { _settings = fmap f (_settings e) }

--------------------------------------------------------------------------------
data Settings app =
  Settings { _logType     :: !LogType
           , _logFilter   :: !LoggerFilter
           , _appSettings :: !app
           }

--------------------------------------------------------------------------------
instance Functor Settings where
  fmap f s = s { _appSettings = f (_appSettings s) }

--------------------------------------------------------------------------------
-- Main Lambda monad stack.
newtype Lambda settings a =
  Lambda { unLambda :: ReaderT (Env settings) IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadFix
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadBase IO
           , MonadBaseControl IO
           )

--------------------------------------------------------------------------------
instance MonadLogger (Lambda settings) where
  monadLoggerLog loc src lvl msg = Lambda $
    do ref <- _loggerRef <$> ask
       liftIO $ loggerCallback ref loc src lvl (toLogStr msg)

--------------------------------------------------------------------------------
instance MonadReader settings (Lambda settings) where
  ask = Lambda (fmap (_appSettings . _settings) ask)
  local k (Lambda m) = Lambda (local (fmap k) m)

--------------------------------------------------------------------------------
lambdaMain :: settings -> Lambda settings () -> IO ()
lambdaMain s (Lambda m) =
  do env <- loadEnv
     runReaderT m env
  where loadEnv =
          Env settings <$> newLoggerRef logType logLevel detailed

        settings = Settings logType logLevel s

        logType = LogStdout 0

        logLevel = LoggerLevel LevelDebug

        detailed = True

