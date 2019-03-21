module LambdaTower.Replay.Update where

import LambdaTower.Loop
import LambdaTower.Types

import qualified LambdaTower.Ingame.GameEvents as IE
import qualified LambdaTower.Ingame.GameState as G
import qualified LambdaTower.Ingame.Update as IU
import qualified LambdaTower.Replay.ReplayState as RS

replayUpdate :: Updater IO RS.ReplayState RS.ReplayState [IE.ControlEvent]
replayUpdate _ _ (RS.ReplayState gameState []) = return $ Left $ RS.ReplayState gameState []
replayUpdate timer controlEvents state = do
  let gameState = RS.state state
  let events:eventStore = RS.events state
  eitherState <- IU.update timer (IE.GameEvents controlEvents events) gameState
  case eitherState of
    Left gameState' -> return $ Left $ RS.ReplayState (G.state gameState') eventStore
    Right gameState' -> return $ Right $ RS.ReplayState gameState' eventStore
