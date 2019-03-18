module LambdaTower.Ingame.Layer (
  LambdaTower.Screen.Position,
  LambdaTower.Screen.Size,
  Layer(..),
  ground,
  posY
) where

import LambdaTower.Screen

data Layer = Layer {
  id :: Int,
  size :: Size,
  position :: Position,
  origin :: Position
}

ground :: Layer
ground = Layer 0 (1000, 80) (0, 80) (0, 80)

posY :: Layer -> Float
posY layer = let (_, y) = position layer in y