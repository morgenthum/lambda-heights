module LambdaTower.Pause.Update where

import LambdaTower.Loop

import qualified LambdaTower.Types.Button as Button
import qualified LambdaTower.Types.ButtonList as ButtonList
import qualified LambdaTower.Types.KeyEvents as Events
import qualified LambdaTower.Types.PauseState as State

update :: Updater IO State.PauseState State.ExitReason [Events.KeyEvent]
update _ events state = do
  let buttonList = ButtonList.ensureValidIndex $ Events.applyEvents (State.buttonList state) events
  let newState = state { State.buttonList = buttonList }
  return $
    if ButtonList.action buttonList
      then Left $ stateByButton $ ButtonList.selectedButton buttonList
      else Right newState

stateByButton :: Button.Button -> State.ExitReason
stateByButton button =
  case Button.text button of
    "exit" -> State.Exit
    _      -> State.Resume