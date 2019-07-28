module LambdaHeights.Types.Layer where

import           LambdaHeights.Types
import           Linear.V2

data Layer = Layer {
  layerId  :: Int,
  entryId  :: Int,
  size     :: Size,
  position :: Position
}

ground :: Layer
ground = Layer {layerId = 0, entryId = 0, size = V2 1000 50, position = V2 0 50}

posX :: Layer -> Float
posX layer = let V2 x _ = position layer in x

posY :: Layer -> Float
posY layer = let V2 _ y = position layer in y
