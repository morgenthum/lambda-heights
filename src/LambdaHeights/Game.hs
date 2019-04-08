module LambdaHeights.Game
  ( start
  )
where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan

import           Control.Monad

import           LambdaHeights.Graphics
import           LambdaHeights.Loop
import           LambdaHeights.Serialize

import           System.Directory

import qualified LambdaHeights.Ingame                    as Ingame
import qualified LambdaHeights.Menu                      as Menu
import qualified LambdaHeights.Pause                     as Pause
import qualified LambdaHeights.Replay                    as Replay
import qualified LambdaHeights.Score                     as Score

import qualified LambdaHeights.Types.Events              as Events
import qualified LambdaHeights.Types.GameState           as Game
import qualified LambdaHeights.Types.IngameState         as Ingame
import qualified LambdaHeights.Types.MenuState           as Menu
import qualified LambdaHeights.Types.PauseState          as Pause
import qualified LambdaHeights.Types.Player              as Ingame
import qualified LambdaHeights.Types.ReplayState         as Replay
import qualified LambdaHeights.Types.ScoreState          as Score
import qualified LambdaHeights.Types.Timer               as Timer

type IngameLoopState = LoopState IO Ingame.State Ingame.Result
type PauseLoopState = LoopState IO (Pause.State Ingame.State) Pause.ExitReason

defaultTimer :: IO Timer.LoopTimer
defaultTimer = Timer.newTimer 7

defaultReplayFilePath :: String
defaultReplayFilePath = "replay.dat"

start :: IO ()
start = do
  graphics <- newGraphics "Lambda-Heights"
  _        <- startState graphics Game.Menu
  deleteGraphics graphics

startState :: Graphics -> Game.State -> IO Game.State
startState _        Game.Exit   = return Game.Exit
startState graphics Game.Menu   = startMenu graphics >>= startState graphics
startState graphics Game.Ingame = startGame graphics >>= startState graphics
startState graphics Game.Replay =
  startReplay defaultReplayFilePath graphics >>= startState graphics

startMenu :: Graphics -> IO Game.State
startMenu graphics = do
  timer  <- defaultTimer
  config <- Menu.defaultConfig

  let loop = timedLoop Menu.keyInput Menu.update noOutput (Menu.render graphics config)
  state <- startLoop timer Menu.newState loop

  Menu.deleteConfig config
  return state

startGame :: Graphics -> IO Game.State
startGame graphics = do
  channel        <- newTChanIO
  ingameConfig   <- Ingame.defaultConfig
  pauseConfig    <- Pause.defaultConfig

  replayFilePath <- Replay.fileName
  safeDeleteFile replayFilePath

  let ingameRenderer = Ingame.renderDefault graphics ingameConfig
  let pauseRenderer = Pause.render graphics pauseConfig $ Ingame.renderPause graphics ingameConfig

  let gameLoop = timedLoop Ingame.keyInput Ingame.update (Ingame.output channel) ingameRenderer
  let pauseLoop      = timedLoop Menu.keyInput Pause.update noOutput pauseRenderer

  state <- startGameLoop replayFilePath channel Ingame.newState gameLoop pauseLoop
    >>= showScore graphics

  Pause.deleteConfig pauseConfig
  Ingame.deleteConfig ingameConfig
  return state

startGameLoop
  :: FilePath
  -> TChan (Maybe [Events.PlayerEvent])
  -> Ingame.State
  -> IngameLoopState
  -> PauseLoopState
  -> IO Score.Score
startGameLoop replayFilePath channel gameState ingameLoop pauseLoop = do
  timer  <- defaultTimer
  handle <- async $ serialize (fromTChan channel) (toFile replayFilePath)
  result <- startLoop timer gameState ingameLoop
  wait handle
  let score = Ingame.score . Ingame.player . Ingame.state $ result
  case Ingame.reason result of
    Ingame.Finished -> return score
    Ingame.Pause    -> do
      reason <- startLoop timer (Pause.newState $ Ingame.state result) pauseLoop
      case reason of
        Pause.Resume ->
          startGameLoop replayFilePath channel (Ingame.state result) ingameLoop pauseLoop
        Pause.Exit -> return score

showScore :: Graphics -> Score.Score -> IO Game.State
showScore graphics score = do
  timer  <- defaultTimer
  config <- Score.defaultConfig

  let loop = timedLoop Menu.keyInput Score.update noOutput $ Score.render graphics config
  _ <- startLoop timer (Score.newState score) loop

  Score.deleteConfig config
  return Game.Menu

startReplay :: FilePath -> Graphics -> IO Game.State
startReplay replayFilePath graphics = do
  serialized <- deserializeFromFile replayFilePath
  case serialized of
    Nothing     -> return Game.Menu
    Just events -> do
      timer  <- defaultTimer
      config <- Ingame.defaultConfig

      let loop = timedLoop Replay.input Replay.update noOutput $ Replay.render graphics config
      result <- startLoop timer (Replay.State Ingame.newState events) loop
      _ <- showScore graphics $ Ingame.score $ Ingame.player $ Replay.state result

      Ingame.deleteConfig config
      return Game.Menu

safeDeleteFile :: FilePath -> IO ()
safeDeleteFile filePath = do
  exist <- doesFileExist filePath
  when exist $ removeFile filePath
