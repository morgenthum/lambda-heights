module LambdaTower.Replay.Update where

import qualified LambdaTower.Ingame.GameEvents as Ingame
import qualified LambdaTower.Ingame.GameState  as Ingame
import qualified LambdaTower.Ingame.Update     as Ingame
import qualified LambdaTower.Timing.Timer      as Timer
import qualified LambdaTower.Replay.State      as Replay

update :: Timer.LoopTimer -> [Ingame.ControlEvent] -> Replay.State -> IO (Either Replay.State Replay.State)
update _     _             (Replay.State gameState []) = return $ Left $ Replay.State gameState []
update timer controlEvents state                       = do
  let gameState           = Replay.state state
  let events : eventStore = Replay.events state
  eitherState <- Ingame.update timer (Ingame.GameEvents controlEvents events) gameState
  case eitherState of
    Left  gameState' -> return $ Left $ Replay.State (Ingame.state gameState') eventStore
    Right gameState' -> return $ Right $ Replay.State gameState' eventStore
