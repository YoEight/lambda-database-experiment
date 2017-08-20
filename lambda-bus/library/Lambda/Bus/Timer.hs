{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Lambda.Bus.Timer
-- Copyright :  (C) 2017 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Lambda.Bus.Timer
  ( TimerPlanning(..)
  , configureTimer
  , registerTimer
  ) where

--------------------------------------------------------------------------------
import Lambda.Prelude

--------------------------------------------------------------------------------
import Lambda.Bus.Builder
import Lambda.Bus.Types

--------------------------------------------------------------------------------
data TimerState =
  TimerState
  { _timerStopped :: IORef Bool }

--------------------------------------------------------------------------------
configureTimer :: Configure settings ()
configureTimer = initialize go
  where
    go = do
      self <- TimerState <$> newIORef False
      subscribe (onRegisterTimer self)

--------------------------------------------------------------------------------
onRegisterTimer :: TimerState -> RegisterTimer -> React settings ()
onRegisterTimer self (RegisterTimer evt duration oneOff) =
  delayed self evt duration oneOff

--------------------------------------------------------------------------------
data RegisterTimer =
  forall e. Typeable e => RegisterTimer e NominalDiffTime Bool

--------------------------------------------------------------------------------
data TimerPlanning = OnOff | Undefinitely

--------------------------------------------------------------------------------
registerTimer :: Typeable evt
              => evt
              -> NominalDiffTime
              -> TimerPlanning
              -> React settings ()
registerTimer evt period plan =
  publish (RegisterTimer evt period boolean)
    where boolean =
            case plan of
              OnOff ->
                True
              Undefinitely ->
                False

--------------------------------------------------------------------------------
delayed :: Typeable e
        => TimerState
        -> e
        -> NominalDiffTime
        -> Bool
        -> React settings ()
delayed TimerState{..} msg timespan oneOff = void $ fork loop
  where
    micros = truncate (timespan * s2mcs)
    loop = do
      threadDelay micros
      publish msg
      stopped <- readIORef _timerStopped
      unless (oneOff || stopped) loop

