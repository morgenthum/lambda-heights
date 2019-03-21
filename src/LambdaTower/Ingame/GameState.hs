module LambdaTower.Ingame.GameState where

import LambdaTower.Ingame.Layer
import LambdaTower.Ingame.Player
import LambdaTower.Screen

data ExitReason = Exit | Pause

data GameResult = GameResult {
  state :: GameState,
  reason :: ExitReason
}

data GameState = GameState {
  time :: Integer,
  screen :: Screen,
  motion :: Motion,
  player :: Player,
  layers :: [Layer]
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
  screen = newScreen,
  motion = newMotion,
  player = newPlayer,
  layers = []
}

newMotion :: Motion
newMotion = Motion {
  moveLeft = False,
  moveRight = False,
  jump = False,
  air = False
}
