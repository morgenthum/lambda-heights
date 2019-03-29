module LambdaTower.Loop where

import qualified SDL

import qualified Control.Lens                  as L

import qualified Control.Monad.Fail            as M
import qualified Control.Monad.State           as M

import qualified LambdaTower.Types.Timer       as Timer

type LoopState m s r = M.StateT (Timer.TimedState s r) m ()

type InputHandler m e = m e
type Updater m s r e = Timer.LoopTimer -> e -> s -> m (Either r s)
type Renderer m s = Timer.LoopTimer -> s -> m ()

startLoop :: (M.MonadFail m, M.MonadIO m) => Timer.LoopTimer -> s -> LoopState m s r -> m r
startLoop timer state loop = do
  Left result <- L.view Timer.state <$> M.execStateT loop (Timer.TimedState timer $ Right state)
  return result

timedLoop :: (M.MonadIO m) => InputHandler m e -> Updater m s r e -> Renderer m s -> LoopState m s r
timedLoop handleInput update render = do
  updateTimer
  updateFrameCounter
  inputAndUpdate handleInput update
  timedState <- M.get
  let timer = L.view Timer.timer timedState
  let state = L.view Timer.state timedState
  case state of
    Left  _         -> return ()
    Right gameState -> do
      M.lift $ render timer gameState
      incrementFrame
      timedLoop handleInput update render

incrementFrame :: (M.Monad m) => LoopState m s r
incrementFrame = M.modify (L.over (Timer.timer . Timer.counter . Timer.frameCount) (+ 1))

updateTimer :: (M.MonadIO m) => LoopState m s r
updateTimer = do
  timedState <- M.get
  current    <- fromIntegral <$> SDL.ticks

  let timer   = L.view Timer.timer timedState
  let elapsed = current - L.view Timer.current timer
  let lag     = L.view Timer.lag timer + elapsed

  M.put $ L.set (Timer.timer . Timer.current) current $ L.set (Timer.timer . Timer.elapsed) elapsed $ L.set
    (Timer.timer . Timer.lag)
    lag
    timedState

updateFrameCounter :: (M.MonadIO m) => LoopState m s r
updateFrameCounter = do
  timedState <- M.get
  let timer          = L.view Timer.timer timedState
  let counter        = L.view Timer.counter timer

  let elapsedMillis = L.view Timer.current timer - L.view Timer.countStart counter
  let elapsedSeconds = realToFrac elapsedMillis / 1000 :: Float
  let frameCount     = L.view Timer.frameCount counter
  let fps = round (realToFrac frameCount / realToFrac elapsedSeconds :: Float)

  M.when (elapsedSeconds >= 0.25 && frameCount > 10)
    $ M.put
    $ L.set (Timer.timer . Timer.counter . Timer.countStart) (L.view Timer.current timer)
    $ L.set (Timer.timer . Timer.counter . Timer.frameCount) 0
    $ L.set (Timer.timer . Timer.counter . Timer.fps) fps timedState

inputAndUpdate :: (M.MonadIO m) => InputHandler m e -> Updater m s r e -> LoopState m s r
inputAndUpdate handleInput update = do
  timedState <- M.get
  case L.view Timer.state timedState of
    Left  _         -> return ()
    Right gameState -> do
      let timer = L.view Timer.timer timedState
      M.when (L.view Timer.lag timer > L.view Timer.rate timer) $ do
        events       <- M.lift handleInput
        newGameState <- M.lift $ update timer events gameState
        let lag  = L.view Timer.lag timer
        let rate = L.view Timer.rate timer
        M.put $ L.set (Timer.timer . Timer.lag) (lag - rate) $ L.set Timer.state newGameState timedState
        inputAndUpdate handleInput update
