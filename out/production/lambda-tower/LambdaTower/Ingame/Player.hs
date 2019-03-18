module LambdaTower.Ingame.Player (
  LambdaTower.Screen.Position,
  Score,
  Velocity,
  Acceleration,
  Player(..),
  newPlayer
) where

import LambdaTower.Screen

type Score = Int
type Velocity = (Float, Float)
type Acceleration = (Float, Float)

data Player = Player {
  score :: Score,
  position :: Position,
  velocity :: Velocity,
  acceleration :: Acceleration
}

newPlayer :: Player
newPlayer = Player {
  score = 0,
  position = (500, 80),
  velocity = (0, 0),
  acceleration = (0, 0)
}
