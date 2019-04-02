module LambdaTower.Game
  ( start
  )
where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan

import           Control.Monad

import           LambdaTower.Graphics
import           LambdaTower.Serialization
import           LambdaTower.State
import           LambdaTower.Types
import           LambdaTower.Timing.Loop

import           System.Directory

import qualified LambdaTower.Ingame.Events     as Ingame
import qualified LambdaTower.Ingame.State      as Ingame
import qualified LambdaTower.Ingame.Input      as Ingame
import qualified LambdaTower.Ingame.Player     as Ingame
import qualified LambdaTower.Ingame.Render     as Ingame
import qualified LambdaTower.Ingame.Update     as Ingame

import qualified LambdaTower.Menu.Input        as Menu
import qualified LambdaTower.Menu.Render       as Menu
import qualified LambdaTower.Menu.State        as Menu
import qualified LambdaTower.Menu.Update       as Menu

import qualified LambdaTower.Pause.Render      as Pause
import qualified LambdaTower.Pause.State       as Pause
import qualified LambdaTower.Pause.Update      as Pause

import qualified LambdaTower.Replay.Input      as Replay
import qualified LambdaTower.Replay.Render     as Replay
import qualified LambdaTower.Replay.State      as Replay
import qualified LambdaTower.Replay.Update     as Replay

import qualified LambdaTower.Score.Render      as Score
import qualified LambdaTower.Score.State       as Score
import qualified LambdaTower.Score.Update      as Score

import qualified LambdaTower.Timing.Timer      as Timer

type IngameLoopState = LoopState IO Ingame.State Ingame.Result
type PauseLoopState = LoopState IO (Pause.State Ingame.State) Pause.ExitReason

defaultReplayFilePath :: String
defaultReplayFilePath = "replay.dat"

start :: IO ()
start = do
  graphics <- newGraphics "LambdaTower"
  _        <- startState graphics Menu
  deleteGraphics graphics

startState :: Graphics -> State -> IO State
startState _        Exit   = return Exit
startState graphics Menu   = startMenu graphics >>= startState graphics
startState graphics Ingame = startGame defaultReplayFilePath graphics >>= startState graphics
startState graphics Replay = startReplay defaultReplayFilePath graphics >>= startState graphics

startMenu :: Graphics -> IO State
startMenu graphics = do
  timer  <- Timer.defaultTimer
  config <- Menu.defaultConfig

  let loop = timedLoop Menu.keyInput Menu.update (Menu.render graphics config)
  state <- startLoop timer Menu.newMenuState loop

  Menu.deleteConfig config
  return state

startGame :: FilePath -> Graphics -> IO State
startGame replayFilePath graphics = do
  channel      <- newTChanIO
  ingameConfig <- Ingame.defaultConfig
  pauseConfig  <- Pause.defaultConfig
  safeDeleteFile replayFilePath

  let ingameRenderer = Ingame.renderDefault graphics ingameConfig
  let pauseRenderer = Pause.render graphics pauseConfig $ Ingame.renderPause graphics ingameConfig

  let gameLoop       = timedLoop Ingame.keyInput (Ingame.updateAndWrite channel) ingameRenderer
  let pauseLoop      = timedLoop Menu.keyInput Pause.update $ pauseRenderer

  state <- startGameLoop replayFilePath channel Ingame.newState gameLoop pauseLoop
    >>= showScore graphics

  Pause.deleteConfig pauseConfig
  Ingame.deleteConfig ingameConfig
  return state

startGameLoop
  :: FilePath
  -> Channel Ingame.PlayerEvent
  -> Ingame.State
  -> IngameLoopState
  -> PauseLoopState
  -> IO Score.Score
startGameLoop replayFilePath channel gameState ingameLoop pauseLoop = do
  timer  <- Timer.defaultTimer
  handle <- async $ serializeFromTChanToFile replayFilePath channel
  result <- startLoop timer gameState ingameLoop
  wait handle
  let score = Ingame.score . Ingame.player . Ingame.state $ result
  case Ingame.reason result of
    Ingame.Finished -> return score
    Ingame.Pause    -> do
      reason <- startLoop timer (Pause.newPauseState $ Ingame.state result) pauseLoop
      case reason of
        Pause.Resume ->
          startGameLoop replayFilePath channel (Ingame.state result) ingameLoop pauseLoop
        Pause.Exit -> return score

showScore :: Graphics -> Score.Score -> IO State
showScore graphics score = do
  timer  <- Timer.defaultTimer
  config <- Score.defaultConfig

  let loop = timedLoop Menu.keyInput Score.update $ Score.render graphics config
  _ <- startLoop timer (Score.newScoreState score) loop

  Score.deleteConfig config
  return Menu

startReplay :: FilePath -> Graphics -> IO State
startReplay replayFilePath graphics = do
  serialized <- deserializeFromFile replayFilePath
  case serialized of
    Nothing     -> return Menu
    Just events -> do
      timer  <- Timer.defaultTimer
      config <- Ingame.defaultConfig

      let loop = timedLoop Replay.keyInput Replay.update $ Replay.render graphics config
      _ <- startLoop timer (Replay.State Ingame.newState events) loop

      Ingame.deleteConfig config
      return Menu

safeDeleteFile :: FilePath -> IO ()
safeDeleteFile filePath = do
  exist <- doesFileExist filePath
  when exist $ removeFile filePath
