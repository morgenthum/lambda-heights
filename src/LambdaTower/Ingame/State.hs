{-# LANGUAGE DeriveGeneric #-}

module LambdaTower.Ingame.State where

import Codec.Serialise

import Data.Word

import GHC.Generics

import LambdaTower.Ingame.Layer
import LambdaTower.Ingame.Player

data GameState = GameState {
  begin :: Word32,
  view :: View,
  motion :: Motion,
  player :: Player,
  layers :: [Layer]
} deriving (Show, Generic)

instance Serialise GameState

data View = View {
  top :: Float,
  left :: Float,
  bottom :: Float,
  right :: Float
} deriving (Show, Generic)

instance Serialise View

data Motion = Motion {
  moveLeft :: Bool,
  moveRight :: Bool,
  jump :: Bool,
  air :: Bool
} deriving (Show, Generic)

instance Serialise Motion

newGameState :: Word32 -> GameState
newGameState begin = GameState {
  begin = begin,
  view = newView,
  motion = newMotion,
  player = newPlayer,
  layers = []
}

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
