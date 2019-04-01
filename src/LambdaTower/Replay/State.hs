module LambdaTower.Replay.State where

import           LambdaTower.Ingame.GameEvents
import           LambdaTower.Ingame.GameState

data State = State {
  state :: GameState,
  events :: [[PlayerEvent]]
}
