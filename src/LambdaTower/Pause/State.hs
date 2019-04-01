module LambdaTower.Pause.State where

import           LambdaTower.Screen
import           LambdaTower.Types.Button
import           LambdaTower.Types.ButtonList

import qualified LambdaTower.Ingame.GameState  as Ingame

data State = State {
  state :: Ingame.GameState,
  buttonList :: ButtonList,
  reason :: Maybe ExitReason
}

data ExitReason = Exit | Resume

newPauseState :: Ingame.GameState -> State
newPauseState s = State
  { state      = s
  , buttonList = newButtonList newScreen [Button 0 "resume" (500, 550), Button 1 "exit" (500, 450)]
  , reason     = Nothing
  }
