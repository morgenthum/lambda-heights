module LambdaTower.Ingame.GameState where

import Data.Word

import LambdaTower.Ingame.Layer
import LambdaTower.Ingame.Player
import LambdaTower.Screen

data GameState = GameState {
  begin :: Word32,
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

newGameState :: Word32 -> GameState
newGameState millis = GameState {
  begin = millis,
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
