module LambdaTower.Ingame.Layer where

import Linear.V2

type Size = V2 Float
type Position = V2 Float

data Layer = Layer {
  id :: Int,
  size :: Size,
  position :: Position
} deriving Show

ground :: Layer
ground = Layer 0 (V2 1000 80) (V2 0 80)

posY :: Layer -> Float
posY layer = let (V2 _ y) = position layer in y