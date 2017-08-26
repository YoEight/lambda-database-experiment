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
  , TimerState(..)
  , Timer(..)
  , registerTimer
  , onRegisterTimer
  ) where

--------------------------------------------------------------------------------
import Lambda.Prelude

--------------------------------------------------------------------------------
import Lambda.Bus.Types

--------------------------------------------------------------------------------
data TimerState =
  TimerState
  { _timerStopped :: IORef Bool }

--------------------------------------------------------------------------------
onRegisterTimer :: TimerState -> RegisterTimer -> React settings ()
onRegisterTimer self (RegisterTimer evt duration oneOff) =
  delayed self evt duration oneOff

--------------------------------------------------------------------------------
data Timer =
  forall a. Typeable a =>
  Timer { timerEvent    :: !a
        , timerPeriod   :: !NominalDiffTime
        , timerPlanning :: !TimerPlanning
        }

--------------------------------------------------------------------------------
data RegisterTimer =
  forall e. Typeable e => RegisterTimer e NominalDiffTime Bool

--------------------------------------------------------------------------------
data TimerPlanning = OnOff | Undefinitely

--------------------------------------------------------------------------------
registerTimer :: (Typeable evt, PubSub p)
              => p settings
              -> UUID
              -> evt
              -> NominalDiffTime
              -> TimerPlanning
              -> Lambda settings ()
registerTimer p uid evt period plan =
  publishOn p uid (RegisterTimer evt period boolean)
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

