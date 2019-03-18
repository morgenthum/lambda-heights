module LambdaTower.Screen where

import Foreign.C.Types

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
}

newScreen :: Screen
newScreen = Screen {
  top = 1000,
  left = 0,
  bottom = 0,
  right = 1000
}

toWindowSize :: Screen -> WindowSize -> Size -> WindowSize
toWindowSize screen (SDL.V2 w h) (x, y) = SDL.V2 (round x') (round y')
  where x' = x * fromIntegral w / (right screen - left screen)
        y' = y * fromIntegral h / (top screen - bottom screen)

toWindowPosition :: Screen -> WindowSize -> Position -> WindowPosition
toWindowPosition screen (SDL.V2 w h) (x, y) = SDL.V2 x' y'
  where x' = translateX screen w x
        y' = translateY screen h y

translateX :: (Integral a) => Screen -> a -> Float -> a
translateX screen w = round . (* fromIntegral w) . normalize (left screen, right screen)

translateY :: (Integral a) => Screen -> a -> Float -> a
translateY screen h = round . (* fromIntegral h) . flipRange . normalize (bottom screen, top screen)

normalize :: (Fractional a) => (a, a) -> a -> a
normalize (minRange, maxRange) x = (x - minRange) / (maxRange - minRange)

flipRange :: (Fractional a) => a -> a
flipRange x = 1 - x
