module LambdaTower.Types.ReplayState where

import LambdaTower.Types.GameEvents
import LambdaTower.Types.GameState

data ReplayState = ReplayState {
  state :: GameState,
  events :: [[PlayerEvent]]
}