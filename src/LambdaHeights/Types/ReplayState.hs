{-# LANGUAGE DeriveGeneric #-}

module LambdaHeights.Types.ReplayState where

import           Data.Time
import           Data.Word
import           Data.Yaml
import           GHC.Generics
import           LambdaHeights.Types.Events
import qualified LambdaHeights.Types.PlayState as Play
import           LambdaHeights.Version

data Result = Result {
  reason :: Play.ExitReason,
  state  :: State
}

data State = State {
  playState :: Play.State,
  events    :: [[PlayerEvent]]
}

data Description = Description {
  fileName :: String,
  time     :: LocalTime,
  duration :: Word32,
  score    :: Int,
  version  :: Version
} deriving (Eq, Generic)

instance ToJSON Description
instance FromJSON Description

instance Ord Description where
 lhs <= rhs = time lhs <= time rhs

