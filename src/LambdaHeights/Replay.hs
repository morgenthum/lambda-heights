module LambdaHeights.Replay where

import           Data.Time
import qualified LambdaHeights.Play              as Play
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.Events      as Events
import qualified LambdaHeights.Types.PlayState   as Play
import qualified LambdaHeights.Types.ReplayState as Replay
import qualified LambdaHeights.Types.Timer       as Timer

input :: IO [Events.ControlEvent]
input = Events.control <$> Play.keyInput

update
  :: Timer.LoopTimer -> [Events.ControlEvent] -> Replay.State -> Either Replay.State Replay.State
update _ _ (Replay.State state []) = Left $ Replay.State state []
update timer controlEvents state =
  let events : eventsList = Replay.events state
      updated = Play.update timer (Events.Events controlEvents events) $ Replay.state state
  in  case updated of
        Left  result    -> Left $ Replay.State (Play.state result) eventsList
        Right playState -> Right $ Replay.State playState eventsList

render :: RenderContext -> Play.RenderConfig -> Timer.LoopTimer -> Replay.State -> IO ()
render graphics config timer = Play.renderDefault graphics config timer . Replay.state

newFileName :: UTCTime -> String
newFileName = (++ ".replay") . formatTime defaultTimeLocale "%_Y%m%d%H%M%S"
