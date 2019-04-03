module LambdaTower.Types.Layer where

import           LambdaTower.Types

data Layer = Layer {
  layerId :: Int,
  entryId :: Int,
  size :: Size,
  position :: Position
}

ground :: Layer
ground = Layer {layerId = 0, entryId = 0, size = (1000, 50), position = (0, 50)}

posX :: Layer -> Float
posX layer = let (x, _) = position layer in x

posY :: Layer -> Float
posY layer = let (_, y) = position layer in y
