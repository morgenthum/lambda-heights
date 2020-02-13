module LambdaHeights.Types.Screen
  ( module LambdaHeights.Vectors,
    Screen (..),
    newScreen,
    top,
    left,
    bottom,
    right,
  )
where

import LambdaHeights.Vectors

data Screen
  = Screen
      { pos :: ScreenPos,
        size :: ScreenSize
      }

newScreen :: Screen
newScreen = Screen (SP (V2 0 0)) (SS (V2 1000 1000))

top :: Screen -> Float
top screen =
  let SP (V2 _ y) = pos screen
      SS (V2 _ h) = size screen
   in y + h

left :: Screen -> Float
left screen = let SP (V2 x _) = pos screen in x

bottom :: Screen -> Float
bottom screen = let SP (V2 _ y) = pos screen in y

right :: Screen -> Float
right screen =
  let SP (V2 x _) = pos screen
      SS (V2 w _) = size screen
   in x + w
