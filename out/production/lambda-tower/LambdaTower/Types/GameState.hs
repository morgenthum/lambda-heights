module LambdaTower.Types.GameState where

import qualified LambdaTower.Types.Layer as Layer
import qualified LambdaTower.Types.Player as Player
import qualified LambdaTower.Screen as Screen

data GameResult = GameResult {
  state :: GameState,
  reason :: ExitReason
}

data ExitReason = Exit | Pause

data GameState = GameState {
  time :: Integer,
  screen :: Screen.Screen,
  motion :: Motion,
  player :: Player.Player,
  layers :: [Layer.Layer]
}

data Motion = Motion {
  moveLeft :: Bool,
  moveRight :: Bool,
  jump :: Bool,
  air :: Bool
}

newGameState :: GameState
newGameState = GameState {
  time = 0,
  screen = Screen.newScreen,
  motion = newMotion,
  player = Player.newPlayer,
  layers = []
}

newMotion :: Motion
newMotion = Motion {
  moveLeft = False,
  moveRight = False,
  jump = False,
  air = False
}
