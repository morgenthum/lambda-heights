module LambdaHeights.Replay
  ( filePath
  , input
  , update
  , render
  )
where

import           Control.Monad.State
import           Data.Time
import qualified LambdaHeights.Play              as Play
import           LambdaHeights.Render
import           LambdaHeights.RenderContext
import           LambdaHeights.Scale
import qualified LambdaHeights.Types.Events      as Events
import qualified LambdaHeights.Types.Loop        as Loop
import qualified LambdaHeights.Types.PlayState   as Play
import qualified LambdaHeights.Types.ReplayState as Replay
import qualified LambdaHeights.Types.Timer       as Timer
import           Linear.V4

filePath :: UTCTime -> String
filePath time = "replays/" ++ formatTime defaultTimeLocale "%_Y%m%d%H%M%S" time

input :: IO [Events.ControlEvent]
input = Events.control <$> Play.keyInput

update :: Loop.Update Replay.State Replay.Result [Events.ControlEvent]
update events = do
  timer <- Loop.getTimer
  state <- Loop.getState
  case state of
    (Replay.State state []) -> Loop.putResult $ Replay.Result Play.Finished $ Replay.State state []
    _                       -> do
      Loop.putTimer $ updateTimer timer state
      let repEvents : repEventList = Replay.events state
      let playUpdate               = Play.update $ Events.Events events repEvents
      let playState                = (timer, Right $ Replay.playState state)
      let (_, updated)             = execState playUpdate playState
      case updated of
        Left playResult -> do
          let
            result = Replay.Result (Play.reason playResult)
              $ Replay.State (Play.state playResult) repEventList
          Loop.putResult result
        Right playState -> Loop.putState $ Replay.State playState repEventList

updateTimer :: Timer.LoopTimer -> Replay.State -> Timer.LoopTimer
updateTimer timer state =
  let remainingFrames = length $ Replay.events state
      go n | n < 200   = timer { Timer.rate = 14 }
           | otherwise = timer
  in  go remainingFrames

render :: RenderContext -> Play.RenderConfig -> Timer.LoopTimer -> Replay.State -> IO ()
render ctx config timer state = do
  Play.render ctx config timer $ Replay.playState state
  let remainingFrames = length $ Replay.events state
  when (remainingFrames <= 50) $ do
    let a = flipRange (normalize (0, 50) (realToFrac remainingFrames)) * 255 :: Float
    renderOverlay ctx $ V4 0 0 0 $ truncate a
