{-# LANGUAGE TemplateHaskell #-}

module LambdaHeights.Resources where

import           Control.Monad.IO.Class
import           Data.FileEmbed
import qualified SDL.Font               as SDLF

highSchoolUSASansFont :: (MonadIO m) => Int -> m SDLF.Font
highSchoolUSASansFont = SDLF.decode $(embedFile "fonts/HighSchoolUSASans.ttf")

retroGamingFont :: (MonadIO m) => Int -> m SDLF.Font
retroGamingFont = SDLF.decode $(embedFile "fonts/retro_gaming.ttf")
