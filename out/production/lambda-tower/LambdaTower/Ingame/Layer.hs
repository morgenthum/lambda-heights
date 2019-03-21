module LambdaTower.Ingame.Layer where

import LambdaTower.Types

data Layer = Layer {
  id :: Int,
  size :: Size,
  position :: Position
}

ground :: Layer
ground = Layer 0 (1000, 50) (0, 50)

posX :: Layer -> Float
posX layer = let (x, _) = position layer in x

posY :: Layer -> Float
posY layer = let (_, y) = position layer in y