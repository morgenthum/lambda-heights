module LambdaTower.Types.MenuState where

import           LambdaTower.Screen
import           LambdaTower.UserInterface

newtype MenuState = State {
  buttonList :: ButtonList
}

newState :: MenuState
newState = State
  { buttonList = newButtonList
    newScreen
    [Button 0 "play" (500, 600), Button 1 "replay" (500, 500), Button 2 "exit" (500, 400)]
  }
