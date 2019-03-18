{-# LANGUAGE DeriveGeneric #-}

module LambdaTower.Ingame.GameState where

import Codec.Serialise

import Data.Word

import GHC.Generics

import LambdaTower.Ingame.Layer
import LambdaTower.Ingame.Player
import LambdaTower.Screen

data GameState = GameState {
  begin :: Word32,
  view :: Screen,
  motion :: Motion,
  player :: Player,
  layers :: [Layer]
} deriving (Show, Generic)

instance Serialise GameState

data Motion = Motion {
  moveLeft :: Bool,
  moveRight :: Bool,
  jump :: Bool,
  air :: Bool
} deriving (Show, Generic)

instance Serialise Motion

newGameState :: Word32 -> GameState
newGameState millis = GameState {
  begin = millis,
  view = newScreen,
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
