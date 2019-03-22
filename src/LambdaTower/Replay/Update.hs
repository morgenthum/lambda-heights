module LambdaTower.Replay.Update where

import LambdaTower.Loop

import qualified LambdaTower.Ingame.Update as Ingame
import qualified LambdaTower.Types.GameEvents as Events
import qualified LambdaTower.Types.GameState as Game
import qualified LambdaTower.Types.ReplayState as Replay

replayUpdate :: Updater IO Replay.ReplayState Replay.ReplayState [Events.ControlEvent]
replayUpdate _ _ (Replay.ReplayState gameState []) = return $ Left $ Replay.ReplayState gameState []
replayUpdate timer controlEvents state = do
  let gameState = Replay.state state
  let events:eventStore = Replay.events state
  eitherState <- Ingame.update timer (Events.GameEvents controlEvents events) gameState
  case eitherState of
    Left gameState' -> return $ Left $ Replay.ReplayState (Game.state gameState') eventStore
    Right gameState' -> return $ Right $ Replay.ReplayState gameState' eventStore
