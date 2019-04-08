module LambdaHeights.Scale where

import           Foreign.C.Types

import           LambdaHeights.Types
import           LambdaHeights.Types.Screen

import qualified SDL

type WindowPosition = SDL.V2 CInt
type WindowSize = SDL.V2 CInt

toWindowSize :: Screen -> WindowSize -> Size -> WindowSize
toWindowSize screen (SDL.V2 w h) (x, y) =
  let x' = translate screen w x
      y' = translate screen h y
  in  SDL.V2 x' y'

toWindowPosition :: Screen -> WindowSize -> Position -> WindowPosition
toWindowPosition screen (SDL.V2 w h) (x, y) =
  let x' = translate screen w x
      y' = translateFlipped screen h y
  in  SDL.V2 x' y'

translate :: (Integral a) => Screen -> a -> Float -> a
translate screen w = round . (* fromIntegral w) . normalize (left screen, right screen)

translateFlipped :: (Integral a) => Screen -> a -> Float -> a
translateFlipped screen h =
  round . (* fromIntegral h) . flipRange . normalize (bottom screen, top screen)

normalize :: (Fractional a) => (a, a) -> a -> a
normalize (minRange, maxRange) x = (x - minRange) / (maxRange - minRange)

flipRange :: (Fractional a) => a -> a
flipRange x = 1 - x
