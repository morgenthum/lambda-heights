module LambdaTower.Types.ReplayState where

import           LambdaTower.Ingame.Events
import           LambdaTower.Types.IngameState

data ReplayState = ReplayState {
  state :: IngameState,
  events :: [[PlayerEvent]]
}
