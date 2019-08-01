module LambdaHeights.Play.Output
  ( output
  )
where

import qualified ComposeEngine.Types.Loop        as Loop
import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Time
import           Data.Yaml
import           LambdaHeights.Types.Events
import qualified LambdaHeights.Types.Player      as Player
import qualified LambdaHeights.Types.PlayState   as State
import qualified LambdaHeights.Types.ReplayState as Replay
import           LambdaHeights.Version

type Output = Loop.Output IO State.State State.Result Events

-- | Outputs occured events into a channel and
--   creates a replay description file if the end is reached.
output :: LocalTime -> FilePath -> TChan (Maybe [PlayerEvent]) -> Output
output time fileName channel events = do
  state <- Loop.askOutputState
  liftIO $ atomically $ writeTChan channel $ Just $ player events
  case state of
    Left result -> liftIO $ do
      atomically $ writeTChan channel Nothing
      encodeFile (fileName ++ ".desc") $ toJSON $ createDesc time fileName result
    Right _ -> return ()

createDesc :: LocalTime -> FilePath -> State.Result -> Replay.Description
createDesc time fileName result = Replay.Description
  { Replay.fileName = fileName ++ ".dat"
  , Replay.time     = time
  , Replay.duration = State.duration $ State.state result
  , Replay.score    = Player.score $ State.player $ State.state result
  , Replay.version  = currentVersion
  }
