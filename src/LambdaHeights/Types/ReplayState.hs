{-# LANGUAGE DeriveGeneric #-}

module LambdaHeights.Types.ReplayState where

import           Data.Time
import           Data.Word
import           Data.Yaml
import           GHC.Generics
import           LambdaHeights.Types.Events
import qualified LambdaHeights.Types.PlayState as Play

data State = State {
  state  :: Play.State,
  events :: [[PlayerEvent]]
}

data Description = Description {
  fileName :: String,
  time     :: UTCTime,
  duration :: Word32,
  score    :: Int
} deriving Generic

instance ToJSON Description
instance FromJSON Description
