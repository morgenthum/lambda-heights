{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed
import qualified LambdaHeights.Game as Game
import LambdaHeights.Types.Config
import qualified SDL.Font as SDLF

main :: IO ()
main = do
  Game.init
  config <- Config <$> highSchoolUSASansFont 28 <*> retroGamingFont 11
  Game.start config
  Game.destroy

highSchoolUSASansFont :: Int -> IO SDLF.Font
highSchoolUSASansFont = SDLF.decode $(embedFile "../fonts/HighSchoolUSASans.ttf")

retroGamingFont :: Int -> IO SDLF.Font
retroGamingFont = SDLF.decode $(embedFile "../fonts/retro_gaming.ttf")
