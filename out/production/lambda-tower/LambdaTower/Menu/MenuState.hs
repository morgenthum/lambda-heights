module LambdaTower.Menu.MenuState where

import LambdaTower.Screen

data Button = Button {
  id :: Int,
  text :: String,
  position :: Position
}

data MenuState = MenuState {
  view :: View,
  buttons :: [Button],
  selected :: Int,
  action :: Bool
}

newMenuState :: MenuState
newMenuState = MenuState {
  view = newView,
  buttons = [
    Button 0 "play" (500, 600),
    Button 1 "replay" (500, 500),
    Button 2 "exit" (500, 400)
  ],
  selected = 0,
  action = False
}