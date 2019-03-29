module LambdaTower.Score.Update where

import           LambdaTower.Loop

import qualified LambdaTower.Types.ButtonList  as ButtonList
import qualified LambdaTower.Types.KeyEvents   as Events
import qualified LambdaTower.Types.ScoreState  as State

update :: Updater IO State.ScoreState () [Events.KeyEvent]
update _ events state = do
  let buttonList = ButtonList.ensureValidIndex $ Events.applyEvents (State.buttonList state) events
  return $ if ButtonList.action buttonList then Left () else Right state

wrap :: State.ScoreState -> ButtonList.ButtonList -> State.ScoreState
wrap state buttonList = state { State.buttonList = buttonList }
