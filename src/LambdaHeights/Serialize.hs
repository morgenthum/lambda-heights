module LambdaHeights.Serialize where

import           Codec.Serialise

import           Control.Concurrent.STM.TChan
import           Control.Monad.Extra
import           Control.Monad.STM

import           System.Directory

import qualified Data.ByteString.Lazy                    as BS
import qualified Data.ByteString.Lazy.Char8              as BS8

type Source m a = m (Maybe a)
type Target m a = a -> m ()

fromTChan :: TChan (Maybe a) -> Source IO a
fromTChan = atomically . readTChan

toFile :: (Serialise a) => FilePath -> Target IO a
toFile path x = do
  BS.appendFile path $ serialise x
  BS.appendFile path $ BS8.pack "/"

serialize :: (Monad m, Serialise a) => Source m a -> Target m a -> m ()
serialize source target = do
  maybeX <- source
  whenJust maybeX $ \x -> do
    target x
    serialize source target

deserializeFromFile :: (Serialise a) => FilePath -> IO (Maybe [a])
deserializeFromFile filePath = do
  exist <- doesFileExist filePath
  if exist
    then do
      bytes <- BS.readFile filePath
      let splitted = filter (not . BS.null) $ BS8.split '/' bytes
      return $ Just $ map deserialise splitted
    else return Nothing
