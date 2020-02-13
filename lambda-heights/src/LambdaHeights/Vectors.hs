{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LambdaHeights.Vectors
  ( module Linear.V2,
    ScreenPosF (..),
    ScreenSizeF (..),
    WorldAccF (..),
    WorldVelF (..),
    WorldPosF (..),
    WorldSizeF (..),
    ScreenPos,
    ScreenSize,
    WorldAcc,
    WorldVel,
    WorldPos,
    WorldSize,
  )
where

import Linear.V2

newtype ScreenPosF a = SP (V2 a) deriving (Show, Eq, Ord, Functor, Applicative)

newtype ScreenSizeF a = SS (V2 a) deriving (Show, Eq, Ord, Functor, Applicative)

newtype WorldAccF a = WA (V2 a) deriving (Show, Eq, Ord, Functor, Applicative)

newtype WorldVelF a = WV (V2 a) deriving (Show, Eq, Ord, Functor, Applicative)

newtype WorldPosF a = WP (V2 a) deriving (Show, Eq, Ord, Functor, Applicative)

newtype WorldSizeF a = WS (V2 a) deriving (Show, Eq, Ord, Functor, Applicative)

type ScreenPos = ScreenPosF Float

type ScreenSize = ScreenSizeF Float

type WorldAcc = WorldAccF Float

type WorldVel = WorldVelF Float

type WorldPos = WorldPosF Float

type WorldSize = WorldSizeF Float
