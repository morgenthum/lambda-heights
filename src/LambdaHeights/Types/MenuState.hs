module LambdaHeights.Types.MenuState where

import           LambdaHeights.Types.Button
import           LambdaHeights.Types.ButtonList
import           LambdaHeights.Types.Screen

newtype State = State {
  buttonList :: ButtonList
}

newState :: State
newState = State
  { buttonList = newButtonList
                   newScreen
                   [ Button 0 "play"   (500, 600)
                   , Button 1 "replay" (500, 500)
                   , Button 2 "exit"   (500, 400)
                   ]
  }
