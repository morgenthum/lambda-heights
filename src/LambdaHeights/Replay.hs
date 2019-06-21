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
import qualified LambdaHeights.Types.Loop as Loop

filePath :: UTCTime -> String
filePath time = "replays/" ++ formatTime defaultTimeLocale "%_Y%m%d%H%M%S" time

input :: IO [Events.ControlEvent]
input = Events.control <$> Play.keyInput

update :: Loop.Update Replay.State Replay.Result [Events.ControlEvent]
update timer _ (Replay.State state []) = (timer, Left $ Replay.Result Play.Finished $ Replay.State state [])
update timer events state =
  let repEvents : repEventList = Replay.events state
      (_, updated) = Play.update timer (Events.Events events repEvents) $ Replay.playState state
      timer' = updateTimer timer state
  in  case updated of
        Left result ->
          (timer, Left $ Replay.Result (Play.reason result) $ Replay.State (Play.state result) repEventList)
        Right playState -> (timer', Right $ Replay.State playState repEventList)

updateTimer :: Timer.LoopTimer -> Replay.State -> Timer.LoopTimer
updateTimer timer state =
  let remainingFrames = length $ Replay.events state
      go n | n <= 100 = timer { Timer.rate = 14 }
           | n <= 1000 = timer { Timer.rate = 7 }
           | otherwise = timer
  in go remainingFrames

render :: RenderContext -> Play.RenderConfig -> Timer.LoopTimer -> Replay.State -> IO ()
render graphics config timer = Play.render graphics config timer . Replay.playState
