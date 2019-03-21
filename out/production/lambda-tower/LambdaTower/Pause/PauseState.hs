module LambdaTower.Pause.PauseState where

import qualified LambdaTower.Components.Button as B
import qualified LambdaTower.Components.ButtonList as BL
import qualified LambdaTower.Ingame.GameState as IS
import qualified LambdaTower.Screen as S

data PauseState = PauseState {
  state :: IS.GameState,
  buttonList :: BL.ButtonList,
  reason :: Maybe ExitReason
}

data ExitReason = Exit | Resume

newPauseState :: IS.GameState -> PauseState
newPauseState s = PauseState {
  state = s,
  buttonList = BL.newButtonList S.newScreen [
    B.Button 0 "resume" (500, 550),
    B.Button 1 "exit" (500, 450)
  ],
  reason = Nothing
}