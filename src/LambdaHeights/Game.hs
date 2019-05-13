module LambdaHeights.Game
  ( start
  )
where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Data.Time
import           LambdaHeights.Loop
import qualified LambdaHeights.MainMenu              as MainMenu
import qualified LambdaHeights.Menu                  as Menu
import qualified LambdaHeights.Pause                 as Pause
import qualified LambdaHeights.Play                  as Play
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Replay                as Replay
import qualified LambdaHeights.ReplayMenu            as ReplayMenu
import qualified LambdaHeights.Score                 as Score
import           LambdaHeights.Serialize
import qualified LambdaHeights.Types.Events          as Events
import qualified LambdaHeights.Types.GameState       as Game
import qualified LambdaHeights.Types.MainMenuState   as MainMenu
import qualified LambdaHeights.Types.PauseState      as Pause
import qualified LambdaHeights.Types.Player          as Play
import qualified LambdaHeights.Types.PlayState       as Play
import qualified LambdaHeights.Types.ReplayMenuState as ReplayMenu
import qualified LambdaHeights.Types.ReplayState     as Replay
import qualified LambdaHeights.Types.ScoreState      as Score
import qualified LambdaHeights.Types.Timer           as Timer

type PlayLoopState = LoopState IO Play.State Play.Result
type PauseLoopState = LoopState IO (Pause.State Play.State) Pause.ExitReason

defaultTimer :: IO Timer.LoopTimer
defaultTimer = Timer.newTimer 7

start :: IO ()
start = do
  ctx <- newContext "Lambda-Heights"
  _   <- startState ctx Game.Menu
  deleteContext ctx

startState :: RenderContext -> Game.State -> IO Game.State
startState _   Game.Exit   = return Game.Exit
startState ctx Game.Menu   = startMenu ctx >>= startState ctx
startState ctx Game.Play   = startGame ctx >>= startState ctx
--startState ctx Game.Replay = startReplay defaultReplayFilePath ctx >>= startState ctx
startState ctx Game.Replay = startReplayMenu ctx >>= startState ctx

startMenu :: RenderContext -> IO Game.State
startMenu ctx = do
  let menuState = MainMenu.newState
  timer  <- defaultTimer
  config <- Menu.createConfig $ MainMenu.menu menuState

  let loop = timedLoop Menu.keyInput MainMenu.update noOutput (MainMenu.render ctx config)
  state <- startLoop timer menuState loop

  Menu.deleteConfig config
  return state

startGame :: RenderContext -> IO Game.State
startGame ctx = do
  time        <- getCurrentTime
  channel     <- newTChanIO
  playConfig  <- Play.createConfig
  pauseConfig <- Pause.defaultConfig $ Pause.menu $ Pause.newState ()

  let playRenderer  = Play.renderDefault ctx playConfig
  let pauseRenderer = Pause.render ctx pauseConfig $ Play.renderPause ctx playConfig

  let replayFile    = Replay.newFileName time
  let gameLoop =
        timedLoop Play.keyInput Play.update (Play.output time replayFile channel) playRenderer
  let pauseLoop = timedLoop Menu.keyInput Pause.update noOutput pauseRenderer

  state <- startGameLoop replayFile channel Play.newState gameLoop pauseLoop >>= showScore ctx

  Pause.deleteConfig pauseConfig
  Play.deleteConfig playConfig
  return state

startGameLoop
  :: FilePath
  -> TChan (Maybe [Events.PlayerEvent])
  -> Play.State
  -> PlayLoopState
  -> PauseLoopState
  -> IO Score.Score
startGameLoop replayFilePath channel gameState playLoop pauseLoop = do
  timer  <- defaultTimer
  handle <- async $ serialize (fromTChan channel) (toFile replayFilePath)
  result <- startLoop timer gameState playLoop
  wait handle
  let score = Play.score . Play.player . Play.state $ result
  case Play.reason result of
    Play.Finished -> return score
    Play.Paused   -> do
      reason <- startLoop timer (Pause.newState $ Play.state result) pauseLoop
      case reason of
        Pause.Resume -> startGameLoop replayFilePath channel (Play.state result) playLoop pauseLoop
        Pause.Exit   -> return score

showScore :: RenderContext -> Score.Score -> IO Game.State
showScore ctx score = do
  timer  <- defaultTimer
  config <- Menu.createConfig $ Pause.menu $ Pause.newState ()

  let loop = timedLoop Menu.keyInput Score.update noOutput $ Score.render ctx config
  _ <- startLoop timer (Score.newState score) loop

  Menu.deleteConfig config
  return Game.Menu

startReplayMenu :: RenderContext -> IO Game.State
startReplayMenu ctx = do
  timer  <- defaultTimer
  table  <- ReplayMenu.buildTable <$> ReplayMenu.loadReplayFiles
  config <- ReplayMenu.createConfig table

  let loop = timedLoop Menu.keyInput ReplayMenu.update noOutput $ ReplayMenu.render ctx config
  x     <- startLoop timer (ReplayMenu.State table) loop
  state <- if null x then return Game.Menu else startReplay x ctx

  Menu.deleteConfig config
  return state

startReplay :: FilePath -> RenderContext -> IO Game.State
startReplay replayFilePath ctx = do
  replay <- deserializeFromFile replayFilePath
  case replay of
    Nothing     -> return Game.Menu
    Just events -> do
      timer  <- defaultTimer
      config <- Play.createConfig

      let loop = timedLoop Replay.input Replay.update noOutput $ Replay.render ctx config
      result <- startLoop timer (Replay.State Play.newState events) loop
      _      <- showScore ctx $ Play.score $ Play.player $ Replay.state result

      Play.deleteConfig config
      return Game.Menu
