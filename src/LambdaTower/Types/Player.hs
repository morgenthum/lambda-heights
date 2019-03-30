module LambdaTower.Types.Player where

import           LambdaTower.Types

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
newPlayer = Player { score = 0, position = (500, 50), velocity = (0, 0), acceleration = (0, 0) }
