module LambdaTower.Recorder where

import Codec.Serialise

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Data.List.Split

import System.Directory

import LambdaTower.Ingame.State

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8

recordGameState :: FilePath -> TChan (Maybe GameState) -> IO ()
recordGameState filePath channel = do
  maybeGameState <- atomically $ readTChan channel
  case maybeGameState of
    Just gameState -> do
      BS.appendFile filePath (serialise gameState)
      BS.appendFile filePath $ BS8.pack "\n42"
      recordGameState filePath channel
    Nothing -> return ()

readDemo :: FilePath -> IO [GameState]
readDemo filePath= do
  bytes <- BS.readFile filePath
  let splitted = filter (not . null) . splitOn "\n42" $ BS8.unpack bytes
  return $ map (deserialise . BS8.pack) splitted

safeDeleteFile :: FilePath -> IO ()
safeDeleteFile filePath = do
  exist <- doesFileExist filePath
  when exist $ removeFile filePath