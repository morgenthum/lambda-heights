module LambdaTower.Loop where

import qualified Control.Monad.Fail as CM
import qualified Control.Monad.State as CM

import Data.Word

import qualified SDL

data LoopTimer = LoopTimer {
  rate :: Word32,
  current :: Word32,
  elapsed :: Word32,
  lag :: Word32
}

type TimedState s r = (LoopTimer, Either r s)
type LoopState m s r = CM.StateT (TimedState s r) m ()

type InputHandler m e = m e
type Updater m s r e = LoopTimer -> e -> s -> m (Either r s)
type Renderer m s = s -> m ()

defaultTimer :: IO LoopTimer
defaultTimer = newTimer 7

newTimer :: Word32 -> IO LoopTimer
newTimer timerRate = do
  millis <- fromIntegral <$> SDL.ticks :: IO Word32
  return $ LoopTimer {
    rate = timerRate,
    current = millis,
    elapsed = 0,
    lag = 0
  }

startLoop :: (CM.MonadFail m, CM.MonadIO m) => LoopTimer -> s -> LoopState m s r -> m r
startLoop timer state loop = do
  (_, Left r) <- CM.execStateT loop (timer, Right state)
  return r

timedLoop :: (CM.MonadIO m) => InputHandler m e -> Updater m s r e -> Renderer m s -> LoopState m s r
timedLoop handleInput update render = do
  updateTimer
  inputAndUpdate handleInput update
  eitherState <- CM.gets snd
  case eitherState of
    Left _ -> return ()
    Right state -> do
      CM.lift $ render state
      timedLoop handleInput update render

updateTimer :: (CM.MonadIO m) => LoopState m s r
updateTimer = do
  (timer, state) <- CM.get
  newCurrent <- fromIntegral <$> SDL.ticks
  let millis = newCurrent - current timer
  CM.put (timer { current = newCurrent, elapsed = millis, lag = lag timer + millis }, state)

inputAndUpdate :: (CM.MonadIO m) => InputHandler m e -> Updater m s r e -> LoopState m s r
inputAndUpdate handleInput update = do
  timedState <- CM.get
  case timedState of
    (_, Left _) -> return ()
    (timer, Right state) ->
      CM.when (lag timer > rate timer) $ do
        events <- CM.lift handleInput
        newState <- CM.lift $ update timer events state
        CM.put (timer { lag = lag timer - rate timer}, newState)
        inputAndUpdate handleInput update