{-# LANGUAGE DeriveGeneric #-}

module LambdaTower.Ingame.Player where

import Codec.Serialise

import GHC.Generics

type Score = Int
type Size = (Float, Float)
type Position = (Float, Float)
type Velocity = (Float, Float)
type Acceleration = (Float, Float)

data Player = Player {
  score :: Score,
  size :: Size,
  position :: Position,
  velocity :: Velocity,
  acceleration :: Acceleration
} deriving (Show, Generic)

instance Serialise Player

newPlayer :: Player
newPlayer = Player {
  score = 0,
  size = (40, 80),
  position = (500, 80),
  velocity = (0, 0),
  acceleration = (0, 0)
}