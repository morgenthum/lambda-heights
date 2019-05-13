{-# LANGUAGE DeriveGeneric #-}

module LambdaHeights.Types.ReplayState where

import           Data.Time
import           Data.Word
import           Data.Yaml
import           GHC.Generics
import           LambdaHeights.Types.Events
import qualified LambdaHeights.Types.PlayState as Ingame

data State = State {
  state  :: Ingame.State,
  events :: [[PlayerEvent]]
}

data Description = Description {
  fileName :: String,
  time     :: UTCTime,
  duration :: Word32,
  score    :: Int
} deriving (Generic)

instance ToJSON Description
instance FromJSON Description

toList :: Description -> [String]
toList x =
  let durationSec = realToFrac (duration x) / 1000 :: Float
  in  [fileName x, show $ time x, show durationSec, show $ score x]
