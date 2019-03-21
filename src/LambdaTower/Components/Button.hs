module LambdaTower.Components.Button where

import LambdaTower.Screen

data Button = Button {
  id :: Int,
  text :: String,
  position :: Position
}
