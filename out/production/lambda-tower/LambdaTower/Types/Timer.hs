{-# LANGUAGE TemplateHaskell #-}

module LambdaTower.Types.Timer where

import Control.Lens

import Data.Word

import qualified SDL

data FrameCounter = FrameCounter {
  _countStart :: Word32,
  _frameCount :: Word32,
  _fps :: Word32
}

makeLenses ''FrameCounter

data LoopTimer = LoopTimer {
  _counter :: FrameCounter,
  _rate :: Word32,
  _current :: Word32,
  _elapsed :: Word32,
  _lag :: Word32
}

makeLenses ''LoopTimer

data TimedState s r = TimedState {
  _timer :: LoopTimer,
  _state :: Either r s
}

makeLenses ''TimedState

defaultTimer :: IO LoopTimer
defaultTimer = newTimer 7

newTimer :: Word32 -> IO LoopTimer
newTimer timerRate = do
  millis <- fromIntegral <$> SDL.ticks
  return $ LoopTimer {
    _counter = FrameCounter millis 0 0,
    _rate = timerRate,
    _current = millis,
    _elapsed = 0,
    _lag = 0
  }
