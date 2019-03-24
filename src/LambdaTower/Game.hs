module LambdaTower.Game (
  start
) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TChan

import Control.Monad

import LambdaTower.Graphics
import LambdaTower.Loop
import LambdaTower.Recorder
import LambdaTower.State
import LambdaTower.Types

import qualified LambdaTower.Highscore.Render as H
import qualified LambdaTower.Highscore.Update as H

import qualified LambdaTower.Ingame.Input as I
import qualified LambdaTower.Ingame.Render as I
import qualified LambdaTower.Ingame.Update as I

import qualified LambdaTower.Menu.Input as M
import qualified LambdaTower.Menu.Render as M
import qualified LambdaTower.Menu.Update as M

import qualified LambdaTower.Pause.Render as P
import qualified LambdaTower.Pause.Update as P

import qualified LambdaTower.Replay.Input as R
import qualified LambdaTower.Replay.Render as R
import qualified LambdaTower.Replay.Update as R

import qualified LambdaTower.Types.GameEvents as GE
import qualified LambdaTower.Types.GameState as GS
import qualified LambdaTower.Types.MenuState as MS
import qualified LambdaTower.Types.PauseState as PS
import qualified LambdaTower.Types.Player as P
import qualified LambdaTower.Types.ReplayState as R
import qualified LambdaTower.Types.ScoreState as SS
import qualified LambdaTower.Types.Timer as T

type IngameLoopState = LoopState IO GS.GameState GS.GameResult
type PauseLoopState = LoopState IO PS.PauseState PS.ExitReason

defaultReplayFilePath :: String
defaultReplayFilePath = "replay.dat"

start :: IO ()
start = do
  graphics <- newGraphics "LambdaTower"
  _ <- startState graphics Menu
  deleteGraphics graphics

startState :: Graphics -> State -> IO State
startState _        Exit   = return Exit
startState graphics Menu   = startMenu graphics >>= startState graphics
startState graphics Ingame = startGame defaultReplayFilePath graphics >>= startState graphics
startState graphics Replay = startReplay defaultReplayFilePath graphics >>= startState graphics

startMenu :: Graphics -> IO State
startMenu graphics = do
  timer <- T.defaultTimer
  config <- M.defaultConfig

  let loop = timedLoop M.keyInput M.update (M.render graphics config)
  state <- startLoop timer MS.newMenuState loop

  M.deleteConfig config
  return state

startGame :: FilePath -> Graphics -> IO State
startGame replayFilePath graphics = do
  channel <- newTChanIO
  ingameConfig <- I.defaultConfig
  pauseConfig <- P.defaultConfig

  safeDeleteFile replayFilePath

  let pauseLoop = timedLoop M.keyInput P.update (P.render graphics pauseConfig ingameConfig)
  let gameLoop = timedLoop I.keyInput (I.updateAndWrite channel) (I.defaultRender graphics ingameConfig)
  state <- join $ showScore graphics <$> startGameLoop replayFilePath channel GS.newGameState gameLoop pauseLoop

  P.deleteConfig pauseConfig
  I.deleteConfig ingameConfig
  return state

startGameLoop :: FilePath -> Channel GE.PlayerEvent -> GS.GameState -> IngameLoopState -> PauseLoopState -> IO Int
startGameLoop replayFilePath channel gameState ingameLoop pauseLoop = do
  timer <- T.defaultTimer
  handle <- async $ serializeFromTChanToFile replayFilePath channel
  result <- startLoop timer gameState ingameLoop
  wait handle
  let score = P.score . GS.player . GS.state $ result
  case GS.reason result of
    GS.Pause -> do
      reason <- startLoop timer (PS.newPauseState $ GS.state result) pauseLoop
      case reason of
        PS.Resume -> startGameLoop replayFilePath channel (GS.state result) ingameLoop pauseLoop
        PS.Exit -> return score
    _ -> return score

showScore :: Graphics -> Int -> IO State
showScore graphics score = do
  timer <- T.defaultTimer
  config <- H.defaultConfig

  let loop = timedLoop M.keyInput H.update (H.render graphics config)
  _ <- startLoop timer (SS.newScoreState score) loop

  H.deleteConfig config
  return Menu

startReplay :: FilePath -> Graphics -> IO State
startReplay replayFilePath graphics = do
  maybeStates <- deserializeFromFile replayFilePath

  case maybeStates of
    Nothing -> return Menu
    Just [] -> return Menu
    Just events -> do
      timer <- T.defaultTimer
      config <- I.defaultConfig

      let loop = timedLoop R.keyInput R.replayUpdate (R.renderReplay graphics config)
      _ <- startLoop timer (R.ReplayState GS.newGameState events) loop

      I.deleteConfig config
      return Menu