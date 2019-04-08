module LambdaHeights.Types.Screen where

data Screen = Screen {
  top :: Float,
  left :: Float,
  bottom :: Float,
  right :: Float
}

newScreen :: Screen
newScreen = Screen { top = 1000, left = 0, bottom = 0, right = 1000 }
