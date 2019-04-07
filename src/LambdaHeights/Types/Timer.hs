module LambdaHeights.Types.Timer where

import           Data.Word

import qualified SDL

data FrameCounter = FrameCounter {
  countStart :: Word32,
  frames :: Word32,
  fps :: Word32
}

data LoopTimer = LoopTimer {
  counter :: FrameCounter,
  current :: Word32,
  elapsed :: Word32,
  rate :: Double,
  lag :: Double
}

data TimedState s r = TimedState {
  timer :: LoopTimer,
  state :: Either r s
}

newTimer :: Double -> IO LoopTimer
newTimer r = do
  m <- fromIntegral <$> SDL.ticks
  return $ LoopTimer
    { counter = FrameCounter m 0 0
    , rate    = r
    , current = m
    , elapsed = 0
    , lag     = 0
    }
