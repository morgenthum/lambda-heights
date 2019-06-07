module LambdaHeights.Game
  ( start
  )
where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Monad.Extra
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
import           Prelude                             hiding (init)
import           System.Directory

type PlayLoopState = LoopState IO Play.State Play.Result
type ReplayLoopState = LoopState IO Replay.State Replay.Result

menuTimer :: IO Timer.LoopTimer
menuTimer = Timer.newTimer 30

playTimer :: IO Timer.LoopTimer
playTimer = Timer.newTimer 7

start :: IO ()
start = do
  init
  ctx <- createContext "Lambda-Heights"
  _   <- startState ctx Game.Menu
  deleteContext ctx

init :: IO ()
init = createDirectoryIfMissing True "replays"

startState :: RenderContext -> Game.State -> IO Game.State
startState _   Game.Exit   = return Game.Exit
startState ctx Game.Menu   = startMenu ctx >>= startState ctx
startState ctx Game.Play   = startGame ctx >>= startState ctx
startState ctx Game.Replay = startReplayMenu ctx >>= startState ctx

startMenu :: RenderContext -> IO Game.State
startMenu ctx = do
  timer  <- menuTimer
  config <- Menu.createConfig
  let loop = timedLoop Menu.keyInput MainMenu.update noOutput (MainMenu.render ctx config)
  state <- startLoop timer MainMenu.newState loop
  Menu.deleteConfig config
  return state

startGame :: RenderContext -> IO Game.State
startGame ctx = do
  time       <- getCurrentTime
  channel    <- newTChanIO
  playConfig <- Play.createConfig
  let replayFile    = Replay.newFileName time
  let output        = Play.output time replayFile channel
  let playRenderer  = Play.renderDefault ctx playConfig
  let pauseRenderer = Play.renderPause ctx playConfig
  let gameLoop = timedLoop Play.keyInput Play.update output playRenderer
  state <- startGameLoop ctx replayFile channel Play.newState gameLoop pauseRenderer
    >>= showScore ctx
  Play.deleteConfig playConfig
  return state

startGameLoop
  :: RenderContext
  -> FilePath
  -> TChan (Maybe [Events.PlayerEvent])
  -> Play.State
  -> PlayLoopState
  -> Pause.ProxyRenderer Play.State
  -> IO Score.Score
startGameLoop ctx filePath channel state loop pauseRenderer = do
  timer  <- playTimer
  handle <- async $ serialize (fromTChan channel) (toFile $ filePath ++ ".dat")
  result <- startLoop timer state loop
  let state' = Play.state result
  wait handle
  let score = Play.score $ Play.player state'
  case Play.reason result of
    Play.Finished -> return score
    Play.Paused   -> do
      let pauseState = Pause.newState state'
      reason <- startPause ctx pauseState pauseRenderer
      case reason of
        Pause.Resume -> startGameLoop ctx filePath channel state' loop pauseRenderer
        Pause.Exit   -> return score

startPause :: RenderContext -> Pause.State a -> Pause.ProxyRenderer a -> IO Pause.ExitReason
startPause ctx state proxyRenderer = do
  timer       <- playTimer
  pauseConfig <- Pause.createConfig
  let renderer = Pause.render ctx pauseConfig proxyRenderer
  let loop     = timedLoop Menu.keyInput Pause.update noOutput renderer
  reason <- startLoop timer state loop
  Pause.deleteConfig pauseConfig
  return reason

showScore :: RenderContext -> Score.Score -> IO Game.State
showScore ctx score = do
  timer  <- menuTimer
  config <- Menu.createConfig
  let loop = timedLoop Menu.keyInput Score.update noOutput $ Score.render ctx config
  _ <- startLoop timer (Score.newState score) loop
  Menu.deleteConfig config
  return Game.Menu

startReplayMenu :: RenderContext -> IO Game.State
startReplayMenu ctx = do
  timer  <- menuTimer
  table  <- ReplayMenu.buildTable <$> ReplayMenu.loadReplayFiles
  config <- ReplayMenu.createConfig
  let state = ReplayMenu.newState table
  let loop = timedLoop Menu.keyInput ReplayMenu.update noOutput $ ReplayMenu.render ctx config
  filePath <- startLoop timer state loop
  state    <- maybe (return Game.Menu) (`startReplayFromFile` ctx) filePath
  Menu.deleteConfig config
  return state

startReplayFromFile :: FilePath -> RenderContext -> IO Game.State
startReplayFromFile replayFilePath ctx =
  maybeM (return Game.Menu) (`startReplay` ctx) $ deserializeFromFile replayFilePath

startReplay :: [[Events.PlayerEvent]] -> RenderContext -> IO Game.State
startReplay events ctx = do
  config <- Play.createConfig
  let loop = timedLoop Replay.input Replay.update noOutput $ Replay.render ctx config
  let state         = Replay.State Play.newState events
  let pauseRenderer = Play.renderPause ctx config
  result <- startReplayLoop ctx state loop pauseRenderer >>= showScore ctx
  Play.deleteConfig config
  return result

startReplayLoop
  :: RenderContext
  -> Replay.State
  -> ReplayLoopState
  -> Pause.ProxyRenderer Play.State
  -> IO Score.Score
startReplayLoop ctx state loop pauseRenderer = do
  timer  <- playTimer
  result <- startLoop timer state loop
  let state' = Replay.state result
  let score = Play.score $ Play.player $ Replay.playState state'
  case Replay.reason result of
    Play.Finished -> return score
    Play.Paused   -> do
      let pauseState = Pause.newState $ Replay.playState state'
      reason <- startPause ctx pauseState pauseRenderer
      case reason of
        Pause.Resume -> startReplayLoop ctx state' loop pauseRenderer
        Pause.Exit   -> return score
