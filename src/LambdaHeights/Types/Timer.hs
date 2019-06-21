module LambdaHeights.Types.Timer where

import           Data.Word
import qualified SDL

data LoopTimer = LoopTimer {
  counter :: FrameCounter,
  current :: Word32,
  elapsed :: Word32,
  rate    :: Word32,
  lag     :: Word32
}

data FrameCounter = FrameCounter {
  start  :: Word32,
  frames :: Word32,
  fps    :: Word32
}

newTimer :: Word32 -> IO LoopTimer
newTimer rate = do
  start <- fromIntegral <$> SDL.ticks
  return $ LoopTimer
    { counter = FrameCounter start 0 0
    , rate    = rate
    , current = start
    , elapsed = 0
    , lag     = 0
    }
