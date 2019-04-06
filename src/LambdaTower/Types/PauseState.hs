module LambdaTower.Types.PauseState where

import           LambdaTower.Screen
import           LambdaTower.UserInterface

data State a = State {
  state :: a,
  buttonList :: ButtonList,
  reason :: Maybe ExitReason
}

data ExitReason = Exit | Resume

newState :: a -> State a
newState s = State
  { state      = s
  , buttonList = newButtonList newScreen [Button 0 "resume" (500, 550), Button 1 "exit" (500, 450)]
  , reason     = Nothing
  }
