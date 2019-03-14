{-# LANGUAGE DeriveGeneric #-}

module LambdaTower.Ingame.State where

import Codec.Serialise

import Control.Concurrent.STM.TChan

import Data.Word

import GHC.Generics

import LambdaTower.Ingame.Layer
import LambdaTower.Ingame.Player

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

data State = State {
  begin :: Word32,
  gameState :: GameState
} deriving (Show, Generic)

instance Serialise State

data GameState = GameState {
  view :: View,
  motion :: Motion,
  player :: Player,
  layers :: [Layer]
} deriving (Show, Generic)

instance Serialise GameState

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
  gameState = newGameState
}

newGameState :: GameState
newGameState = GameState {
  view = newView,
  motion = newMotion,
  player = newPlayer,
  layers = []
}
