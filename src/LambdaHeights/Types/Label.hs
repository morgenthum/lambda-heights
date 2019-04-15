module LambdaHeights.Types.Label where

import           LambdaHeights.Types

data Alignment = AlignLeft | AlignCenter

data Label = Label {
  id        :: Int,
  text      :: String,
  position  :: Position,
  alignment :: Alignment
}
