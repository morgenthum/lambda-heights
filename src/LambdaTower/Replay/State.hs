module LambdaTower.Replay.State where

import qualified LambdaTower.Ingame.Events     as Ingame
import qualified LambdaTower.Ingame.State      as Ingame

data State = State {
  state :: Ingame.State,
  events :: [[Ingame.PlayerEvent]]
}
