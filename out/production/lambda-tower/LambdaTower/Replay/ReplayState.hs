module LambdaTower.Replay.ReplayState where

import LambdaTower.Ingame.GameEvents
import LambdaTower.Ingame.GameState

data ReplayState = ReplayState {
  state :: GameState,
  events :: [[PlayerEvent]]
}