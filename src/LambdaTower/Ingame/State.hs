module LambdaTower.Ingame.State where

import Data.Word

import LambdaTower.Ingame.Layer
import LambdaTower.Ingame.Player

data View = View {
  top :: Float,
  left :: Float,
  bottom :: Float,
  right :: Float
} deriving Show

data Motion = Motion {
  moveLeft :: Bool,
  moveRight :: Bool,
  jump :: Bool,
  air :: Bool
} deriving Show

data State = State {
  begin :: Word32,
  view :: View,
  motion :: Motion,
  player :: Player,
  layers :: [Layer]
} deriving Show

newView :: View
newView = View {
  top = 1000,
  left = 0,
  bottom = 0,
  right = 1000
}

newMotion :: Motion
newMotion = Motion {
  moveLeft = False,
  moveRight = False,
  jump = False,
  air = False
}

newState :: Word32 -> State
newState begin = State {
  begin = begin,
  view = newView,
  motion = newMotion,
  player = newPlayer,
  layers = []
}