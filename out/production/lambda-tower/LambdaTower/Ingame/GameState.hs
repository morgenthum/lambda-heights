module LambdaTower.Ingame.GameState where

import qualified LambdaTower.Ingame.Layer as L
import qualified LambdaTower.Ingame.Player as P
import qualified LambdaTower.Screen as S

data GameResult = GameResult {
  state :: GameState,
  reason :: ExitReason
}

data ExitReason = Exit | Pause

data GameState = GameState {
  time :: Integer,
  screen :: S.Screen,
  motion :: Motion,
  player :: P.Player,
  layers :: [L.Layer]
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
  screen = S.newScreen,
  motion = newMotion,
  player = P.newPlayer,
  layers = []
}

newMotion :: Motion
newMotion = Motion {
  moveLeft = False,
  moveRight = False,
  jump = False,
  air = False
}
