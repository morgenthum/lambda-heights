module LambdaTower.Recorder where

import Codec.Serialise

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Data.List.Split

import LambdaTower.Ingame.State

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8

recordGameState :: TChan (Maybe State) -> IO ()
recordGameState channel = do
  maybeGameState <- atomically $ readTChan channel
  case maybeGameState of
    Just gameState -> do
      BS.appendFile "serialized.demo" (serialise gameState)
      BS.appendFile "serialized.demo" $ BS8.pack "\n42"
      recordGameState channel
    Nothing -> return ()

readDemo :: IO [State]
readDemo = do
  bytes <- BS.readFile "serialized.demo"
  let splitted = filter (not . null) . splitOn "\n42" $ BS8.unpack bytes
  return $ map (deserialise . BS8.pack) splitted