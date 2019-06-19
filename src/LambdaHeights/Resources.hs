{-# LANGUAGE TemplateHaskell #-}

module LambdaHeights.Resources where

import           Data.FileEmbed
import qualified SDL.Font       as SDLF

highSchoolUSASansFont :: Int -> IO SDLF.Font
highSchoolUSASansFont = SDLF.decode $(embedFile "fonts/HighSchoolUSASans.ttf")

retroGamingFont :: Int -> IO SDLF.Font
retroGamingFont = SDLF.decode $(embedFile "fonts/retro_gaming.ttf")
