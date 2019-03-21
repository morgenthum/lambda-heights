module LambdaTower.Menu.Update (
  update
) where

import LambdaTower.Loop

import qualified LambdaTower.Menu.Events as E
import qualified LambdaTower.Menu.MenuState as M
import qualified LambdaTower.State as S

update :: Updater IO M.MenuState S.State [E.KeyEvent]
update _ events state = do
  let newState = ensureValidIndex . applyEvents state $ events
  return $
    if M.action newState
      then Left $ stateByButton $ selectedButton newState
      else Right newState

applyEvents :: M.MenuState -> [E.KeyEvent] -> M.MenuState
applyEvents = foldl applyEvent

applyEvent :: M.MenuState -> E.KeyEvent -> M.MenuState
applyEvent state E.Up = state { M.selected = M.selected state - 1 }
applyEvent state E.Down = state { M.selected = M.selected state + 1 }
applyEvent state E.Enter = state { M.action = True}

ensureValidIndex :: M.MenuState -> M.MenuState
ensureValidIndex state
  | selected < 0 = state { M.selected = 0 }
  | selected > count - 1 = state { M.selected = count - 1 }
  | otherwise = state
  where selected = M.selected state
        count = length $ M.buttons state

selectedButton :: M.MenuState -> M.Button
selectedButton state = M.buttons state !! M.selected state

stateByButton :: M.Button -> S.State
stateByButton button =
  case M.id button of
    0 -> S.Ingame
    1 -> S.Replay
    2 -> S.Exit
    _ -> S.Menu