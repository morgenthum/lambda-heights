module LambdaHeights.Replay where

import           Data.Time
import qualified LambdaHeights.Play              as Play
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.Events      as Events
import qualified LambdaHeights.Types.PlayState   as Play
import qualified LambdaHeights.Types.ReplayState as Replay
import qualified LambdaHeights.Types.Timer       as Timer

newFileName :: UTCTime -> String
newFileName time = "replays/" ++ formatTime defaultTimeLocale "%_Y%m%d%H%M%S" time

input :: IO [Events.ControlEvent]
input = Events.control <$> Play.keyInput

update
  :: Timer.LoopTimer -> [Events.ControlEvent] -> Replay.State -> Either Replay.State Replay.State
update _ _ (Replay.State state []) = Left $ Replay.State state []
update timer events state =
  let repEvents : repEventList = Replay.events state
      updated = Play.update timer (Events.Events events repEvents) $ Replay.state state
  in  case updated of
        Left  result    -> Left $ Replay.State (Play.state result) repEventList
        Right playState -> Right $ Replay.State playState repEventList

render :: RenderContext -> Play.RenderConfig -> Timer.LoopTimer -> Replay.State -> IO ()
render graphics config timer = Play.renderDefault graphics config timer . Replay.state
