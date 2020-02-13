{-# LANGUAGE LambdaCase #-}

module LambdaHeights.Serialize where

import Codec.Serialise
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import System.Directory

type Source m a = m (Maybe a)

type Target m a = a -> m ()

-- | Loops a serialization from a source to a target.
serialize :: (Monad m, Serialise a) => Source m a -> Target m a -> m ()
serialize source target = source >>= \case
  Nothing -> return ()
  Just x -> do
    target $! x
    serialize source target

-- | Reads atomically from a channel.
fromTChan :: TChan (Maybe a) -> Source IO a
fromTChan = atomically . readTChan

-- | Serializes to a file and separates with "/".
toFile :: (Serialise a) => FilePath -> Target IO a
toFile path x = do
  BS.appendFile path (serialise x)
  BS.appendFile path (BS8.pack "/")

-- | Deserializes a "/"-separated sequence of serializations from a file.
deserializeFromFile :: (Serialise a) => FilePath -> IO (Maybe [a])
deserializeFromFile filePath = do
  exist <- doesFileExist filePath
  if exist
    then do
      bytes <- BS.readFile filePath
      let splitted = filter (not . BS.null) (BS8.split '/' bytes)
      return $ Just $ map deserialise splitted
    else return Nothing
