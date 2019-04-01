module LambdaTower.Ingame.GameState where

import qualified LambdaTower.Screen            as Screen
import qualified LambdaTower.Ingame.Layer      as Layer
import qualified LambdaTower.Ingame.Player     as Player

data GameResult = GameResult {
  reason :: ExitReason,
  state :: GameState
}

data ExitReason = Finished | Pause

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
  jump :: Bool
}

newGameState :: GameState
newGameState =
  GameState {time = 0, screen = Screen.newScreen, motion = newMotion, player = Player.newPlayer, layers = []}

newMotion :: Motion
newMotion = Motion {moveLeft = False, moveRight = False, jump = False}
