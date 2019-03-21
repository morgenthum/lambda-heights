module LambdaTower.Replay.Input where

import LambdaTower.Loop

import qualified LambdaTower.Ingame.GameEvents as IE
import qualified LambdaTower.Ingame.Input as II

replayKeyInput ::  InputHandler IO [IE.ControlEvent]
replayKeyInput = do
  gameEvents <- II.ingameKeyInput
  case gameEvents of
    IE.GameEvents controlEvents _ -> return controlEvents