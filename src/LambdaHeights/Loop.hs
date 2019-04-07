module LambdaHeights.Loop where

import qualified SDL

import qualified Control.Monad.Fail            as M
import qualified Control.Monad.State           as M

import qualified LambdaHeights.Timer             as Timer

type LoopState m s r = M.StateT (Timer.TimedState s r) m ()

type InputHandler m e = m e
type Updater m s r e = Timer.LoopTimer -> e -> s -> m (Either r s)
type Renderer m s = Timer.LoopTimer -> s -> m ()

startLoop :: (M.MonadFail m, M.MonadIO m) => Timer.LoopTimer -> s -> LoopState m s r -> m r
startLoop timer state loop = do
  Left result <- Timer.state <$> M.execStateT loop (Timer.TimedState timer $ Right state)
  return result

timedLoop :: (M.MonadIO m) => InputHandler m e -> Updater m s r e -> Renderer m s -> LoopState m s r
timedLoop handleInput update render = do
  updateTimer
  updateFrameCounter
  inputAndUpdate handleInput update
  timedState <- M.get
  let timer = Timer.timer timedState
  let state = Timer.state timedState
  case state of
    Left  _         -> return ()
    Right gameState -> do
      M.lift $ render timer gameState
      incrementFrame
      timedLoop handleInput update render

incrementFrame :: (M.Monad m) => LoopState m s r
incrementFrame = M.modify f
 where
  f s = s
    { Timer.timer =
      (Timer.timer s)
        { Timer.counter = (Timer.counter $ Timer.timer s)
                            { Timer.frameCount = (Timer.frameCount $ Timer.counter $ Timer.timer s)
                                                   + 1
                            }
        }
    }

updateTimer :: (M.MonadIO m) => LoopState m s r
updateTimer = do
  timedState <- M.get
  current    <- fromIntegral <$> SDL.ticks

  let timer   = Timer.timer timedState
  let elapsed = current - Timer.current timer
  let lag     = Timer.lag timer + elapsed

  M.put $ timedState
    { Timer.timer = timer { Timer.current = current, Timer.elapsed = elapsed, Timer.lag = lag }
    }

updateFrameCounter :: (M.MonadIO m) => LoopState m s r
updateFrameCounter = do
  timedState <- M.get
  let timer          = Timer.timer timedState
  let counter        = Timer.counter timer

  let elapsedMillis  = Timer.current timer - Timer.countStart counter
  let elapsedSeconds = realToFrac elapsedMillis / 1000 :: Float
  let frameCount     = Timer.frameCount counter
  let fps = round (realToFrac frameCount / realToFrac elapsedSeconds :: Float)

  M.when (elapsedSeconds >= 0.25 && frameCount > 10) $ M.put $ timedState
    { Timer.timer = timer
                      { Timer.counter = counter { Timer.countStart = Timer.current timer
                                                , Timer.frameCount = 0
                                                , Timer.fps        = fps
                                                }
                      }
    }

inputAndUpdate :: (M.MonadIO m) => InputHandler m e -> Updater m s r e -> LoopState m s r
inputAndUpdate handleInput update = do
  timedState <- M.get
  case Timer.state timedState of
    Left  _         -> return ()
    Right gameState -> do
      let timer = Timer.timer timedState
      M.when (Timer.lag timer > Timer.rate timer) $ do
        events       <- M.lift handleInput
        newGameState <- M.lift $ update timer events gameState
        let lag  = Timer.lag timer
        let rate = Timer.rate timer
        M.put $ timedState { Timer.timer = timer { Timer.lag = lag - rate }
                           , Timer.state = newGameState
                           }
        inputAndUpdate handleInput update
