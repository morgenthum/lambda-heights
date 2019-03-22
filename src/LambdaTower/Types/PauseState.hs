module LambdaTower.Types.PauseState where

import LambdaTower.Types.Button
import LambdaTower.Types.ButtonList
import LambdaTower.Screen

import qualified LambdaTower.Types.GameState as Game

data PauseState = PauseState {
  state :: Game.GameState,
  buttonList :: ButtonList,
  reason :: Maybe ExitReason
}

data ExitReason = Exit | Resume

newPauseState :: Game.GameState -> PauseState
newPauseState s = PauseState {
  state = s,
  buttonList = newButtonList newScreen [
    Button 0 "resume" (500, 550),
    Button 1 "exit" (500, 450)
  ],
  reason = Nothing
}