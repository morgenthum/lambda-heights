module LambdaTower.Game (
  start
) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TChan

import LambdaTower.Graphics
import LambdaTower.Loop
import LambdaTower.Recorder
import LambdaTower.State

import qualified LambdaTower.Ingame.GameEvents as I
import qualified LambdaTower.Ingame.Input as I
import qualified LambdaTower.Ingame.Render as I
import qualified LambdaTower.Ingame.GameState as I
import qualified LambdaTower.Ingame.Update as I

import qualified LambdaTower.Menu.Input as M
import qualified LambdaTower.Menu.MenuState as M
import qualified LambdaTower.Menu.Render as M
import qualified LambdaTower.Menu.Update as M

import qualified LambdaTower.Pause.PauseState as P
import qualified LambdaTower.Pause.Render as P
import qualified LambdaTower.Pause.Update as P

type IngameLoopState = LoopState IO I.GameState I.GameResult
type PauseLoopState = LoopState IO P.PauseState P.ExitReason

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
  timer <- defaultTimer
  config <- M.defaultConfig

  let loop = timedLoop M.handleKeyInput M.update (M.render graphics config)
  state <- startLoop timer M.newMenuState loop

  M.deleteConfig config
  return state

type Channel = TChan (Maybe [I.PlayerEvent])

startGame :: FilePath -> Graphics -> IO State
startGame replayFilePath graphics = do
  channel <- newTChanIO
  ingameConfig <- I.defaultConfig
  pauseConfig <- P.defaultConfig

  safeDeleteFile replayFilePath

  let pauseLoop = timedLoop M.handleKeyInput P.pauseUpdate (P.renderPause graphics pauseConfig ingameConfig)
  let gameLoop = timedLoop I.ingameKeyInput (I.updateAndWrite channel) (I.defaultRender graphics ingameConfig)
  startGameLoop replayFilePath channel I.newGameState gameLoop pauseLoop

  P.deleteConfig pauseConfig
  I.deleteConfig ingameConfig
  return Menu

startGameLoop :: FilePath -> Channel -> I.GameState -> IngameLoopState -> PauseLoopState -> IO ()
startGameLoop replayFilePath channel gameState ingameLoop pauseLoop = do
  timer <- defaultTimer
  handle <- async $ serializeFromTChanToFile replayFilePath channel
  result <- startLoop timer gameState ingameLoop
  wait handle
  case I.reason result of
    I.Pause -> do
      reason <- startLoop timer (P.newPauseState $ I.state result) pauseLoop
      case reason of
        P.Resume -> startGameLoop replayFilePath channel (I.state result) ingameLoop pauseLoop
        P.Exit -> return ()
    _ -> return ()

startReplay :: FilePath -> Graphics -> IO State
startReplay replayFilePath graphics = do
  maybeStates <- deserializeFromFile replayFilePath

  case maybeStates of
    Nothing -> return Menu
    Just [] -> return Menu
    Just events -> do
      timer <- defaultTimer
      config <- I.defaultConfig

      let loop = timedLoop I.replayKeyInput I.replayUpdate (I.renderReplay graphics config)
      _ <- startLoop timer (events, I.newGameState) loop

      I.deleteConfig config
      return Menu