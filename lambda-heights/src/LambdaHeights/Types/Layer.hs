module LambdaHeights.Types.Layer where

import LambdaHeights.Vectors

data Layer
  = Layer
      { layerId :: Int,
        entryId :: Int,
        size :: WorldSize,
        position :: WorldPos
      }

ground :: Layer
ground = Layer {layerId = 0, entryId = 0, size = WS (V2 1000 50), position = WP (V2 0 50)}

posX :: Layer -> Float
posX layer = let WP (V2 x _) = position layer in x

posY :: Layer -> Float
posY layer = let WP (V2 _ y) = position layer in y
