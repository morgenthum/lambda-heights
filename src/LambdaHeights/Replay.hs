module LambdaHeights.Replay
  ( filePath
  , input
  , update
  , render
  )
where

import           Data.Time
import qualified LambdaHeights.Play              as Play
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.Events      as Events
import qualified LambdaHeights.Types.PlayState   as Play
import qualified LambdaHeights.Types.ReplayState as Replay
import qualified LambdaHeights.Types.Timer       as Timer

filePath :: UTCTime -> String
filePath time = "replays/" ++ formatTime defaultTimeLocale "%_Y%m%d%H%M%S" time

input :: IO [Events.ControlEvent]
input = Events.control <$> Play.keyInput

update
  :: Timer.LoopTimer -> [Events.ControlEvent] -> Replay.State -> Either Replay.Result Replay.State
update _ _ (Replay.State state []) = Left $ Replay.Result Play.Finished $ Replay.State state []
update timer events state =
  let repEvents : repEventList = Replay.events state
      updated = Play.update timer (Events.Events events repEvents) $ Replay.playState state
  in  case updated of
        Left result ->
          Left $ Replay.Result (Play.reason result) $ Replay.State (Play.state result) repEventList
        Right playState -> Right $ Replay.State playState repEventList

render :: RenderContext -> Play.RenderConfig -> Timer.LoopTimer -> Replay.State -> IO ()
render graphics config timer = Play.render graphics config timer . Replay.playState
