{-# LANGUAGE DeriveGeneric #-}

module LambdaTower.Screen where

import Codec.Serialise
import Foreign.C.Types
import GHC.Generics

import qualified SDL

type WindowPosition = SDL.V2 CInt
type WindowSize = SDL.V2 CInt

type Position = (Float, Float)
type Size = (Float, Float)

data Screen = Screen {
  top :: Float,
  left :: Float,
  bottom :: Float,
  right :: Float
} deriving (Show, Generic)

instance Serialise Screen

newScreen :: Screen
newScreen = Screen {
  top = 1000,
  left = 0,
  bottom = 0,
  right = 1000
}

toWindowSize :: Screen -> WindowSize -> Size -> WindowSize
toWindowSize view (SDL.V2 w h) (x, y) = SDL.V2 (round x') (round y')
  where x' = x * fromIntegral w / (right view - left view)
        y' = y * fromIntegral h / (top view - bottom view)

toWindowPosition :: Screen -> WindowSize -> Position -> WindowPosition
toWindowPosition view (SDL.V2 w h) (x, y) = SDL.V2 x' y'
  where x' = translateX view w x
        y' = translateY view h y

translateX :: (Integral a) => Screen -> a -> Float -> a
translateX view w = round . (* fromIntegral w) . normalize (left view, right view)

translateY :: (Integral a) => Screen -> a -> Float -> a
translateY view h = round . (* fromIntegral h) . flipRange . normalize (bottom view, top view)

normalize :: (Fractional a) => (a, a) -> a -> a
normalize (minRange, maxRange) x = (x - minRange) / (maxRange - minRange)

flipRange :: (Fractional a) => a -> a
flipRange x = 1 - x
