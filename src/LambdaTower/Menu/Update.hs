module LambdaTower.Menu.Update
  ( update
  )
where

import           LambdaTower.State

import qualified LambdaTower.Menu.State        as Menu
import qualified LambdaTower.Timing.Timer      as Timer
import qualified LambdaTower.Types.Button      as Button
import qualified LambdaTower.Types.ButtonList  as ButtonList
import qualified LambdaTower.Types.KeyEvents   as Events

update :: Timer.LoopTimer -> [Events.KeyEvent] -> Menu.State -> IO (Either State Menu.State)
update _ events state = do
  let list = ButtonList.ensureValidIndex $ ButtonList.applyEvents (Menu.buttonList state) events
  return $ if ButtonList.action list
    then Left $ stateByButton $ ButtonList.selectedButton list
    else Right $ state { Menu.buttonList = list }

stateByButton :: Button.Button -> State
stateByButton button = case Button.text button of
  "play"   -> Ingame
  "replay" -> Replay
  "exit"   -> Exit
  _        -> Menu
