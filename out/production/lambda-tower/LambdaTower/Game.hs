module LambdaTower.Game (
  startGame
) where

import LambdaTower.Ingame.Input
import LambdaTower.Ingame.Renderer
import LambdaTower.Ingame.State
import LambdaTower.Ingame.Update
import LambdaTower.Graphics
import LambdaTower.Loop

import qualified SDL

startGame :: IO ()
startGame = do

  graphics <- newGraphics "LambdaTower" "HighSchoolUSASans.ttf" 14
  timer <- newTimer 7

  let begin = current timer
  let loop = timedLoop keyInputHandler updater (renderer graphics)
  score <- startLoop timer (newState begin) loop

  deleteGraphics graphics

  print score
