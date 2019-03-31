module LambdaTower.Types.Timer where

import           Data.Word

import qualified SDL

data FrameCounter = FrameCounter {
  countStart :: Word32,
  frameCount :: Word32,
  fps :: Word32
}

data LoopTimer = LoopTimer {
  counter :: FrameCounter,
  rate :: Word32,
  current :: Word32,
  elapsed :: Word32,
  lag :: Word32
}

data TimedState s r = TimedState {
  timer :: LoopTimer,
  state :: Either r s
}

defaultTimer :: IO LoopTimer
defaultTimer = newTimer 7

newTimer :: Word32 -> IO LoopTimer
newTimer timerRate = do
  millis <- fromIntegral <$> SDL.ticks
  return $ LoopTimer { counter = FrameCounter millis 0 0
                     , rate    = timerRate
                     , current = millis
                     , elapsed = 0
                     , lag     = 0
                     }
