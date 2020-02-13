module LambdaHeights.Types.Player where

import LambdaHeights.Types.Score
import LambdaHeights.Vectors

data MotionType = Ground | Air

data Player
  = Player
      { score :: Score,
        position :: WorldPos,
        velocity :: WorldVel,
        acceleration :: WorldAcc,
        motionType :: MotionType
      }

newPlayer :: Player
newPlayer = Player
  { score = 0,
    position = WP (V2 500 50),
    velocity = WV (V2 0 0),
    acceleration = WA (V2 0 0),
    motionType = Ground
  }
