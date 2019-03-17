module LambdaTower.Game (
  start
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

data State = Exit | Ingame | Replay

defaultReplayFilePath :: String
defaultReplayFilePath = "replay.dat"

start :: IO ()
start = do
  graphics <- newGraphics "LambdaTower" "HighSchoolUSASans.ttf" 14
  _ <- startState graphics Ingame
  deleteGraphics graphics

startState :: Graphics -> State -> IO State
startState _ Exit = return Exit
startState graphics Ingame = startGame defaultReplayFilePath graphics >>= startState graphics
startState graphics Replay = startReplay defaultReplayFilePath graphics >>= startState graphics

startGame :: FilePath -> Graphics -> IO State
startGame replayFilePath graphics = do
  timer <- newTimer 7
  channel <- newTChanIO

  safeDeleteFile replayFilePath
  handle <- async $ serializeGameStates replayFilePath channel

  let begin = current timer
  let loop = timedLoop keyInputHandler (updater channel) (renderer graphics)
  startLoop timer (newGameState begin) loop >>= print
  wait handle

  return Exit

startReplay :: FilePath -> Graphics -> IO State
startReplay replayFilePath graphics = do
  timer <- newTimer 7
  state:states <- deserializeGameStates replayFilePath

  let loop = timedLoop dummyInputHandler replayUpdater (replayRenderer graphics)
  startLoop timer (state, states) loop >>= print

  return Exit