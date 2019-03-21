module LambdaTower.Menu.MenuState where

import LambdaTower.Components.Button
import LambdaTower.Components.ButtonList
import LambdaTower.Screen

newtype MenuState = MenuState {
  buttonList :: ButtonList
}

newMenuState :: MenuState
newMenuState = MenuState {
  buttonList = newButtonList newScreen [
    Button 0 "play" (500, 600),
    Button 1 "replay" (500, 500),
    Button 2 "exit" (500, 400)
  ]
}