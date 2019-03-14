module LambdaTower.Game (
  startGame,
  startReplay
) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TChan

import LambdaTower.Ingame.Input
import LambdaTower.Ingame.Renderer
import LambdaTower.Ingame.State
import LambdaTower.Ingame.Update
import LambdaTower.Graphics
import LambdaTower.Loop
import LambdaTower.Recorder

import qualified SDL

startGame :: IO ()
startGame = do
  graphics <- newGraphics "LambdaTower" "HighSchoolUSASans.ttf" 14
  timer <- newTimer 7
  channel <- newTChanIO

  handle <- async $ recordGameState channel

  let begin = current timer
  let loop = timedLoop keyInputHandler (updater channel) (renderer graphics)
  print <$> startLoop timer (newState begin) loop

  wait handle

  deleteGraphics graphics


startReplay :: IO ()
startReplay = do
  graphics <- newGraphics "LambdaTower" "HighSchoolUSASans.ttf" 14
  timer <- newTimer 7
  state:states <- readDemo

  let loop = timedLoop dummyInputHandler replay (replayRenderer graphics)
  print <$> startLoop timer (state, states) loop

  deleteGraphics graphics