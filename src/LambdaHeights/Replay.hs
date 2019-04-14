module LambdaHeights.Replay where

import           Data.Time
import qualified LambdaHeights.Play              as Ingame
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.Events      as Events
import qualified LambdaHeights.Types.PlayState   as Ingame
import qualified LambdaHeights.Types.ReplayState as Replay
import qualified LambdaHeights.Types.Timer       as Timer

-- Only the control events are relevant, because we have
-- no player control if we are replaying a game.

input :: IO [Events.ControlEvent]
input = Events.controlEvents <$> Ingame.keyInput


-- Apply the events of one update cycle to the current state.
-- Return the events of the remaining cycles for the next update cycle.

update
  :: Timer.LoopTimer -> [Events.ControlEvent] -> Replay.State -> Either Replay.State Replay.State
update _ _ (Replay.State state []) = Left $ Replay.State state []
update timer controlEvents state =
  let events : eventStore = Replay.events state
      newState = Ingame.update timer (Events.Events controlEvents events) $ Replay.state state
  in  case newState of
        Left  result      -> Left $ Replay.State (Ingame.state result) eventStore
        Right ingameState -> Right $ Replay.State ingameState eventStore


-- Unwrap the game state and apply it to the default ingame renderer.

render :: RenderContext -> Ingame.RenderConfig -> Timer.LoopTimer -> Replay.State -> IO ()
render graphics config timer = Ingame.renderDefault graphics config timer . Replay.state


-- Returns the file name for a new replay.

fileName :: IO String
fileName = (++ ".replay") . formatTime defaultTimeLocale "%_Y%m%d%H%M%S" <$> getCurrentTime
