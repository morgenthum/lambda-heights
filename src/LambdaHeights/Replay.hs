module LambdaHeights.Replay where

import           Data.Time

import           LambdaHeights.Graphics

import qualified LambdaHeights.Ingame                    as Ingame

import qualified LambdaHeights.Types.Events              as Events
import qualified LambdaHeights.Types.IngameState         as Ingame
import qualified LambdaHeights.Types.ReplayState         as Replay
import qualified LambdaHeights.Types.Timer               as Timer

input :: IO [Events.ControlEvent]
input = Events.controlEvents <$> Ingame.keyInput

update
  :: Timer.LoopTimer -> [Events.ControlEvent] -> Replay.State -> Either Replay.State Replay.State
update _ _ (Replay.State gameState []) = Left $ Replay.State gameState []
update timer controlEvents state =
  let events : eventStore = Replay.events state
      eitherState = Ingame.update timer (Events.Events controlEvents events) $ Replay.state state
  in  case eitherState of
        Left  result       -> Left $ Replay.State (Ingame.state result) eventStore
        Right wrappedState -> Right $ Replay.State wrappedState eventStore

render :: Graphics -> Ingame.RenderConfig -> Timer.LoopTimer -> Replay.State -> IO ()
render graphics config timer = Ingame.renderDefault graphics config timer . Replay.state

fileName :: IO String
fileName = (++ ".replay") . formatTime defaultTimeLocale "%_Y%m%d%H%M%S" <$> getCurrentTime
