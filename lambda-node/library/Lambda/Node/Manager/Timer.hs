{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : Lambda.Node.Manager.Timer
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lambda.Node.Manager.Timer
  ( Register(..)
  , new
  ) where

--------------------------------------------------------------------------------
import Lambda.Node.Prelude
import Lambda.Node.Types

--------------------------------------------------------------------------------
data Register = forall e. Typeable e => Register e NominalDiffTime Bool

--------------------------------------------------------------------------------
data Internal =
  Internal { _stopped :: IORef Bool }

--------------------------------------------------------------------------------
new :: PubSub h => h -> IO ()
new mainBus = do
  internal <- Internal <$> newIORef False
  subscribe mainBus (onRegister internal)

--------------------------------------------------------------------------------
delayed :: Typeable e
        => Internal
        -> e
        -> NominalDiffTime
        -> Bool
        -> Server ()
delayed Internal{..} msg timespan oneOff = void $ fork loop
  where
    micros = truncate (timespan * s2mcs)
    loop = do
      liftIO $ threadDelay micros
      publish msg
      stopped <- readIORef _stopped
      unless (oneOff || stopped) loop

--------------------------------------------------------------------------------
onRegister :: Internal -> Register -> Server ()
onRegister self (Register msg duration oneOff) =
  delayed self msg duration oneOff