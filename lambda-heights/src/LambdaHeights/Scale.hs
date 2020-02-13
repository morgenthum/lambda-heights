module LambdaHeights.Scale where

import LambdaHeights.Types.Screen

toWindowSize :: (Integral a) => Screen -> V2 a -> WorldSize -> V2 a
toWindowSize screen (V2 w h) (WS (V2 x y)) =
  let x' = translate screen w x
      y' = translate screen h y
   in V2 x' y'

toWindowPosition :: (Integral a) => Screen -> V2 a -> WorldPos -> V2 a
toWindowPosition screen (V2 w h) (WP (V2 x y)) =
  let x' = translate screen w x
      y' = translateFlipped screen h y
   in V2 x' y'

translate :: (Integral a) => Screen -> a -> Float -> a
translate screen w = round . (* fromIntegral w) . normalize (left screen, right screen)

translateFlipped :: (Integral a) => Screen -> a -> Float -> a
translateFlipped screen h =
  round . (* fromIntegral h) . flipRange . normalize (bottom screen, top screen)

normalize :: (Fractional a) => (a, a) -> a -> a
normalize (minRange, maxRange) x = (x - minRange) / (maxRange - minRange)

flipRange :: (Fractional a) => a -> a
flipRange x = 1 - x
