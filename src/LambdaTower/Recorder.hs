module LambdaTower.Recorder where

import Codec.Serialise

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Data.List.Split

import System.Directory

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8

serializeFromTChanToFile :: (Serialise a) => FilePath -> TChan (Maybe a) -> IO ()
serializeFromTChanToFile filePath channel = do
  maybeGameState <- atomically $ readTChan channel
  case maybeGameState of
    Just gameState -> do
      BS.appendFile filePath (serialise gameState)
      BS.appendFile filePath $ BS8.pack "\n42"
      serializeFromTChanToFile filePath channel
    Nothing -> return ()

deserializeFromFile :: (Serialise a) => FilePath -> IO (Maybe [a])
deserializeFromFile filePath= do
  exist <- doesFileExist filePath
  if exist
  then do
    bytes <- BS.readFile filePath
    let splitted = filter (not . null) . splitOn "\n42" $ BS8.unpack bytes
    return . Just $ map (deserialise . BS8.pack) splitted
  else return Nothing

safeDeleteFile :: FilePath -> IO ()
safeDeleteFile filePath = do
  exist <- doesFileExist filePath
  when exist $ removeFile filePath