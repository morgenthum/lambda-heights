module LambdaHeights.Loop
  ( LoopState
  , Input
  , Update
  , Output
  , Render
  , noOutput
  , startLoop
  , timedLoop
  )
where

import qualified Control.Monad.Fail        as M
import qualified Control.Monad.State       as M
import qualified LambdaHeights.Types.Timer as Timer
import qualified SDL

type LoopState m s r = M.StateT (Timer.TimedState s r) m ()

type Input m e = m e
type Update s r e = Timer.LoopTimer -> e -> s -> Either r s
type Output m s r e = Timer.LoopTimer -> e -> Either r s -> m ()
type Render m s = Timer.LoopTimer -> s -> m ()

noOutput :: (Monad m) => Output m s r e
noOutput _ _ _ = return ()

startLoop :: (M.MonadFail m, M.MonadIO m) => Timer.LoopTimer -> s -> LoopState m s r -> m r
startLoop timer state loop = do
  Left result <- Timer.state <$> M.execStateT loop (Timer.TimedState timer $ Right state)
  return result

timedLoop
  :: (M.MonadIO m) => Input m e -> Update s r e -> Output m s r e -> Render m s -> LoopState m s r
timedLoop input update output render = do
  updateTimer
  updateFrameCounter
  updateCycle input update output
  timedState <- M.get
  case Timer.state timedState of
    Left  _     -> return ()
    Right state -> do
      M.lift $ render (Timer.timer timedState) state
      incrementFrame
      timedLoop input update output render

incrementFrame :: (M.Monad m) => LoopState m s r
incrementFrame = M.modify f
 where
  f s = s
    { Timer.timer =
      (Timer.timer s)
        { Timer.counter =
          (Timer.counter $ Timer.timer s) { Timer.frames = ( Timer.frames
                                                           $ Timer.counter
                                                           $ Timer.timer s
                                                           )
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
  let lag     = Timer.lag timer + fromIntegral elapsed

  M.put $ timedState
    { Timer.timer = timer { Timer.current = current, Timer.elapsed = elapsed, Timer.lag = lag }
    }

updateFrameCounter :: (M.Monad m) => LoopState m s r
updateFrameCounter = do
  timedState <- M.get
  let timer          = Timer.timer timedState
  let counter        = Timer.counter timer

  let elapsedMillis  = Timer.current timer - Timer.start counter
  let elapsedSeconds = realToFrac elapsedMillis / 1000 :: Float
  let frames         = Timer.frames counter
  let fps = round (realToFrac frames / elapsedSeconds :: Float)

  M.when (elapsedSeconds >= 0.25 && frames > 10) $ M.put $ timedState
    { Timer.timer = timer
                      { Timer.counter = counter { Timer.start  = Timer.current timer
                                                , Timer.frames = 0
                                                , Timer.fps    = fps
                                                }
                      }
    }

updateCycle :: (M.Monad m) => Input m e -> Update s r e -> Output m s r e -> LoopState m s r
updateCycle input update output = do
  timedState <- M.get
  case Timer.state timedState of
    Left  _     -> return ()
    Right state -> do
      let timer = Timer.timer timedState
      M.when (Timer.lag timer > Timer.rate timer) $ do
        events <- M.lift input
        let eitherState = update timer events state
        M.lift $ output timer events eitherState
        let lag  = Timer.lag timer
        let rate = Timer.rate timer
        M.put $ timedState { Timer.timer = timer { Timer.lag = lag - rate }
                           , Timer.state = eitherState
                           }
        updateCycle input update output
