module LambdaTower.Types.ReplayState where

import           LambdaTower.Types.Events

import qualified LambdaTower.Types.IngameState as Ingame

data State = State {
  state :: Ingame.State,
  events :: [[PlayerEvent]]
}
