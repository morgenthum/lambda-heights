{-# LANGUAGE DeriveGeneric #-}

module LambdaTower.Ingame.Layer where

import Codec.Serialise

import GHC.Generics

type Size = (Float, Float)
type Position = (Float, Float)

data Layer = Layer {
  id :: Int,
  size :: Size,
  position :: Position
} deriving (Show, Generic)

instance Serialise Layer

ground :: Layer
ground = Layer 0 (1000, 80) (0, 80)

posY :: Layer -> Float
posY layer = let (_, y) = position layer in y