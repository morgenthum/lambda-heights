module LambdaTower.Types.ReplayState where

import           LambdaTower.Types.Events

import qualified LambdaTower.Types.IngameState as Ingame

data ReplayState = ReplayState {
  state :: Ingame.State,
  events :: [[PlayerEvent]]
}
