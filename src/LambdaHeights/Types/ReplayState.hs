module LambdaHeights.Types.ReplayState where

import           LambdaHeights.Types.Events
import qualified LambdaHeights.Types.PlayState as Ingame

data State = State {
  state  :: Ingame.State,
  events :: [[PlayerEvent]]
}
