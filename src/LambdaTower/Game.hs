module LambdaTower.Game (
  start
) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TChan

import Control.Monad

import System.Directory

import LambdaTower.Ingame.Input
import LambdaTower.Ingame.Renderer
import LambdaTower.Ingame.State
import LambdaTower.Ingame.Update
import LambdaTower.Graphics
import LambdaTower.Loop
import LambdaTower.Recorder

import qualified SDL

data State = Menu | Ingame | Replay | Exit

defaultReplayFilePath = "replay.dat"

start :: IO ()
start = do
  graphics <- newGraphics "LambdaTower" "HighSchoolUSASans.ttf" 14
  startState graphics Ingame
  deleteGraphics graphics

startState :: Graphics -> State -> IO State
startState _ Exit = return Exit
startState graphics Ingame = startGame defaultReplayFilePath graphics >>= startState graphics
startState graphics Replay = startReplay defaultReplayFilePath graphics >>= startState graphics

startGame :: FilePath -> Graphics -> IO State
startGame demoFilePath graphics = do
  timer <- newTimer 7
  channel <- newTChanIO

  safeDeleteFile demoFilePath
  handle <- async $ serializeGameStates demoFilePath channel

  let begin = current timer
  let loop = timedLoop keyInputHandler (updater channel) (renderer graphics)
  startLoop timer (newGameState begin) loop >>= print
  wait handle

  return Replay

startReplay :: FilePath -> Graphics -> IO State
startReplay demoFilePath graphics = do
  timer <- newTimer 7
  state:states <- deserializeGameStates demoFilePath

  let loop = timedLoop dummyInputHandler replayUpdater (replayRenderer graphics)
  startLoop timer (state, states) loop >>= print

  return Exit