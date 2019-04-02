module LambdaTower.Replay.Update where

import qualified LambdaTower.Ingame.Events     as Ingame
import qualified LambdaTower.Ingame.State      as Ingame
import qualified LambdaTower.Ingame.Update     as Ingame
import qualified LambdaTower.Timing.Timer      as Timer
import qualified LambdaTower.Replay.State      as Replay

update
  :: Timer.LoopTimer
  -> [Ingame.ControlEvent]
  -> Replay.State
  -> IO (Either Replay.State Replay.State)
update _     _             (Replay.State gameState []) = return $ Left $ Replay.State gameState []
update timer controlEvents state                       = do
  let events : eventStore = Replay.events state
  eitherState <- Ingame.update timer (Ingame.Events controlEvents events) $ Replay.state state
  case eitherState of
    Left  result       -> return $ Left $ Replay.State (Ingame.state result) eventStore
    Right wrappedState -> return $ Right $ Replay.State wrappedState eventStore
