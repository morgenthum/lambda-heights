module LambdaHeights.Serialization where

import           Codec.Serialise

import           Control.Concurrent.STM.TChan
import           Control.Monad.Extra
import           Control.Monad.STM

import           System.Directory

import qualified Data.ByteString.Lazy                    as BS
import qualified Data.ByteString.Lazy.Char8              as BS8

serializeFromTChanToFile :: (Serialise a) => FilePath -> TChan (Maybe a) -> IO ()
serializeFromTChanToFile filePath channel = do
  maybeX <- atomically $ readTChan channel
  whenJust maybeX $ \x -> do
    BS.appendFile filePath $ serialise x
    BS.appendFile filePath $ BS8.pack "/"
    serializeFromTChanToFile filePath channel

deserializeFromFile :: (Serialise a) => FilePath -> IO (Maybe [a])
deserializeFromFile filePath = do
  exist <- doesFileExist filePath
  if exist
    then do
      bytes <- BS.readFile filePath
      let splitted = filter (not . BS.null) $ BS8.split '/' bytes
      return $ Just $ map deserialise splitted
    else return Nothing
