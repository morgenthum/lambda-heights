module LambdaHeights.Types.IngameState where

import           Data.Word

import qualified LambdaHeights.Types.Layer               as Layer
import qualified LambdaHeights.Types.Player              as Player
import qualified LambdaHeights.Types.Screen              as Screen

data Result = Result {
  reason :: ExitReason,
  state :: State
}

data ExitReason = Finished | Pause

data State = State {
  time :: Word32,
  screen :: Screen.Screen,
  motion :: Motion,
  player :: Player.Player,
  layers :: [Layer.Layer]
}

data Motion = Motion {
  moveLeft :: Bool,
  moveRight :: Bool,
  jump :: Bool
}

newState :: State
newState = State { time   = 0
                 , screen = Screen.newScreen
                 , motion = newMotion
                 , player = Player.newPlayer
                 , layers = []
                 }

newMotion :: Motion
newMotion = Motion { moveLeft = False, moveRight = False, jump = False }
