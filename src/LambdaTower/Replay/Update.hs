module LambdaTower.Replay.Update where

import           LambdaTower.Loop

import qualified LambdaTower.Ingame.Update     as Ingame
import qualified LambdaTower.Types.GameEvents  as Events
import qualified LambdaTower.Types.GameState   as State
import qualified LambdaTower.Types.ReplayState as Replay

update :: Updater IO Replay.ReplayState Replay.ReplayState [Events.ControlEvent]
update _     _             (Replay.ReplayState gameState []) = return $ Left $ Replay.ReplayState gameState []
update timer controlEvents state                             = do
  let gameState           = Replay.state state
  let events : eventStore = Replay.events state
  eitherState <- Ingame.update timer (Events.GameEvents controlEvents events) gameState
  case eitherState of
    Left  gameState' -> return $ Left $ Replay.ReplayState (State.state gameState') eventStore
    Right gameState' -> return $ Right $ Replay.ReplayState gameState' eventStore
