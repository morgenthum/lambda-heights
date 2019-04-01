module LambdaTower.Menu.State where

import           LambdaTower.Types.Button
import           LambdaTower.Types.ButtonList
import           LambdaTower.Screen

newtype State = State {
  buttonList :: ButtonList
}

newMenuState :: State
newMenuState = State
  { buttonList = newButtonList newScreen
                               [Button 0 "play" (500, 600), Button 1 "replay" (500, 500), Button 2 "exit" (500, 400)]
  }
