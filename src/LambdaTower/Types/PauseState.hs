module LambdaTower.Types.PauseState where

import           LambdaTower.Screen
import           LambdaTower.UI.Button
import           LambdaTower.UI.ButtonList

data PauseState a = PauseState {
  state :: a,
  buttonList :: ButtonList,
  reason :: Maybe ExitReason
}

data ExitReason = Exit | Resume

newPauseState :: a -> PauseState a
newPauseState s = PauseState
  { state      = s
  , buttonList = newButtonList newScreen [Button 0 "resume" (500, 550), Button 1 "exit" (500, 450)]
  , reason     = Nothing
  }
