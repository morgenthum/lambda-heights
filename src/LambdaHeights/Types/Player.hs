module LambdaHeights.Types.Player where

import           LambdaHeights.Types
import           Linear.V2

type Score = Int
type Velocity = V2 Float
type Acceleration = V2 Float

data MotionType = Ground | Air

data Player = Player {
  score        :: Score,
  position     :: Position,
  velocity     :: Velocity,
  acceleration :: Acceleration,
  motionType   :: MotionType
}

newPlayer :: Player
newPlayer = Player
  { score        = 0
  , position     = V2 500 50
  , velocity     = V2 0 0
  , acceleration = V2 0 0
  , motionType   = Ground
  }
