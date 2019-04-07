module LambdaHeights.Ingame.Output
  ( output
  )
where

import           Control.Concurrent.STM.TChan

import           Control.Monad.IO.Class
import           Control.Monad.STM

import           LambdaHeights.Types.Events
import           LambdaHeights.Types.IngameState
import           LambdaHeights.Types.Timer

type Output = LoopTimer -> Events -> Either Result State -> IO ()

-- Broadcast occured events into a channel.

output :: TChan (Maybe [PlayerEvent]) -> Output
output channel _ events eitherState = do
  liftIO $ atomically $ writeTChan channel $ Just $ playerEvents events
  case eitherState of
    Left  _ -> liftIO $ atomically $ writeTChan channel Nothing
    Right _ -> return ()
