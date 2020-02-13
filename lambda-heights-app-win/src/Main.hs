module Main where

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
highSchoolUSASansFont = SDLF.load "fonts/HighSchoolUSASans.ttf"

retroGamingFont :: Int -> IO SDLF.Font
retroGamingFont = SDLF.load "fonts/retro_gaming.ttf"
