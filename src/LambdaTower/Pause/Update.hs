module LambdaTower.Pause.Update where

import LambdaTower.Loop

import qualified LambdaTower.Types.Button as Button
import qualified LambdaTower.Types.ButtonList as ButtonList
import qualified LambdaTower.Types.KeyEvents as Events
import qualified LambdaTower.Types.PauseState as State

update :: Updater IO State.PauseState State.ExitReason [Events.KeyEvent]
update _ events state = do
  let buttonList = ButtonList.ensureValidIndex $ Events.applyEvents (State.buttonList state) events
  let newState = wrap state buttonList
  return $
    if ButtonList.action buttonList
      then Left $ stateByButton $ ButtonList.selectedButton buttonList
      else Right newState

wrap :: State.PauseState -> ButtonList.ButtonList -> State.PauseState
wrap state buttonList = state { State.buttonList = buttonList }

stateByButton :: Button.Button -> State.ExitReason
stateByButton button =
  case Button.id button of
    1 -> State.Exit
    _ -> State.Resume