{-# LANGUAGE DeriveGeneric #-}

module LambdaHeights.Version where

import Data.List.Split
import Data.Yaml
import GHC.Generics

newtype Version = Version (Int, Int, Int)
  deriving (Eq, Generic)

instance Show Version where
  show (Version (major, minor, patch)) = show major ++ "." ++ show minor ++ "." ++ show patch

instance Read Version where
  readsPrec _ x =
    let [major, minor, patch] = splitOn "." x
        version = Version (read major, read minor, read patch)
     in [(version, "")]

instance ToJSON Version

instance FromJSON Version

currentVersion :: Version
currentVersion = Version (1, 3, 0)
