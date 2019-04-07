module LambdaHeights.Replay where

import           LambdaHeights.Graphics

import qualified LambdaHeights.Ingame            as Ingame
import qualified LambdaHeights.Timer             as Timer

import qualified LambdaHeights.Types.Events      as Events
import qualified LambdaHeights.Types.IngameState as Ingame
import qualified LambdaHeights.Types.ReplayState as Replay

keyInput :: IO [Events.ControlEvent]
keyInput = Events.controlEvents <$> Ingame.keyInput

update
  :: Timer.LoopTimer
  -> [Events.ControlEvent]
  -> Replay.State
  -> IO (Either Replay.State Replay.State)
update _ _ (Replay.State gameState []) = return $ Left $ Replay.State gameState []
update timer controlEvents state = do
  let events : eventStore = Replay.events state
  eitherState <- Ingame.update timer (Events.Events controlEvents events) $ Replay.state state
  case eitherState of
    Left  result       -> return $ Left $ Replay.State (Ingame.state result) eventStore
    Right wrappedState -> return $ Right $ Replay.State wrappedState eventStore

render :: Graphics -> Ingame.RenderConfig -> Timer.LoopTimer -> Replay.State -> IO ()
render graphics config timer = Ingame.renderDefault graphics config timer . Replay.state
