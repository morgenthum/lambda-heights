module LambdaTower.Ingame.Player where

import Linear.V2

type Score = Int
type Size = V2 Float
type Position = V2 Float
type Velocity = V2 Float
type Acceleration = V2 Float

data Player = Player {
  score :: Score,
  size :: Size,
  position :: Position,
  velocity :: Velocity,
  acceleration :: Acceleration
} deriving Show

newPlayer :: Player
newPlayer = Player {
  score = 0,
  size = V2 40 80,
  position = V2 500 80,
  velocity = V2 0 0,
  acceleration = V2 0 0
}