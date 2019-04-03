module LambdaTower.Replay where

import           LambdaTower.Graphics
import qualified LambdaTower.Types.IngameState as Ingame
import qualified LambdaTower.Types.ReplayState as Replay

import qualified LambdaTower.Ingame.Events     as Ingame
import qualified LambdaTower.Ingame.Input      as Ingame
import qualified LambdaTower.Ingame.Render     as Ingame
import qualified LambdaTower.Ingame.Update     as Ingame
import qualified LambdaTower.Timing.Timer      as Timer


-- Input

keyInput :: IO [Ingame.ControlEvent]
keyInput = Ingame.controlEvents <$> Ingame.keyInput


-- Update

update
  :: Timer.LoopTimer
  -> [Ingame.ControlEvent]
  -> Replay.ReplayState
  -> IO (Either Replay.ReplayState Replay.ReplayState)
update _ _ (Replay.ReplayState gameState []) = return $ Left $ Replay.ReplayState gameState []
update timer controlEvents state = do
  let events : eventStore = Replay.events state
  eitherState <- Ingame.update timer (Ingame.Events controlEvents events) $ Replay.state state
  case eitherState of
    Left  result       -> return $ Left $ Replay.ReplayState (Ingame.state result) eventStore
    Right wrappedState -> return $ Right $ Replay.ReplayState wrappedState eventStore


-- Render

render :: Graphics -> Ingame.RenderConfig -> Timer.LoopTimer -> Replay.ReplayState -> IO ()
render graphics config timer = Ingame.renderDefault graphics config timer . Replay.state
