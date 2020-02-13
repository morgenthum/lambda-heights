{-# LANGUAGE LambdaCase #-}

module LambdaHeights.Play.Output
  ( output,
    writeDescription,
    createDescription,
  )
where

import qualified ComposeEngine.Types.Loop as Loop
import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Time
import Data.Yaml
import LambdaHeights.Types.Events
import qualified LambdaHeights.Types.PlayState as State
import qualified LambdaHeights.Types.Player as Player
import qualified LambdaHeights.Types.ReplayState as Replay
import LambdaHeights.Version

type Output = Loop.Output IO State.State State.Result Events

type DescriptionGen = State.Result -> Replay.Description

type DescriptionWriter = State.Result -> IO ()

-- | Outputs occured events into a channel and
--   creates a replay description file if the end is reached.
output :: DescriptionWriter -> TChan (Maybe [PlayerEvent]) -> Output
output descWriter channel events = do
  liftIO $ atomically $ writeTChan channel $ Just (player events)
  Loop.askOutputState >>= \case
    Left result -> liftIO $ do
      atomically $ writeTChan channel Nothing
      descWriter result
    Right _ -> return ()

writeDescription :: FilePath -> DescriptionGen -> DescriptionWriter
writeDescription fileName descGen result = encodeFile (fileName ++ ".desc") $ toJSON (descGen result)

createDescription :: LocalTime -> FilePath -> DescriptionGen
createDescription time fileName result = Replay.Description
  { Replay.fileName = fileName ++ ".dat",
    Replay.time = time,
    Replay.duration = State.duration $ State.state result,
    Replay.score = Player.score $ State.player $ State.state result,
    Replay.version = currentVersion
  }
