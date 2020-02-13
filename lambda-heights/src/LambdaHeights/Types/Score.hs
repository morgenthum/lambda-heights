{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LambdaHeights.Types.Score where

import Data.Yaml
import GHC.Generics

newtype Score = Score Int deriving (Eq, Ord, Num, Generic)

instance ToJSON Score

instance FromJSON Score
