{-# LANGUAGE DeriveGeneric #-}

module LambdaTower.Ingame.Player where

import Codec.Serialise

import GHC.Generics

type Score = Int
type Position = (Float, Float)
type Velocity = (Float, Float)
type Acceleration = (Float, Float)

data Player = Player {
  score :: Score,
  position :: Position,
  velocity :: Velocity,
  acceleration :: Acceleration
} deriving (Show, Generic)

instance Serialise Player

newPlayer :: Player
newPlayer = Player {
  score = 0,
  position = (500, 80),
  velocity = (0, 0),
  acceleration = (0, 0)
}
