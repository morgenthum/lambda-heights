{-# LANGUAGE DeriveGeneric #-}

module LambdaTower.Ingame.GameEvents where

import Codec.Serialise

import GHC.Generics

data GameEvents = GameEvents {
  controlEvents :: [ControlEvent],
  playerEvents :: [PlayerEvent]
}

data ControlEvent = Paused
                  deriving (Eq)

data PlayerEvent = PlayerMoved Direction Bool
                 | PlayerJumped
                 deriving (Eq, Generic)

instance Serialise PlayerEvent

data Direction = MoveLeft
               | MoveRight
               deriving (Eq, Generic)

instance Serialise Direction