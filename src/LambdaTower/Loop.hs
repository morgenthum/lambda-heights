{-# LANGUAGE RankNTypes #-}

module LambdaTower.Loop where

import Control.Monad.Fail
import Control.Monad.State

import Data.Word

import qualified SDL

data LoopTimer = LoopTimer {
  rate :: Word32,
  current :: Word32,
  elapsed :: Word32,
  lag :: Word32
}

type TimedState s r = (LoopTimer, Either s r)
type Loop m s r = StateT (TimedState s r) m ()

type InputHandler m e = m e
type Updater m s r e = LoopTimer -> s -> e -> m (Either s r)
type Renderer m s =  s -> m ()

newTimer :: Word32 -> IO LoopTimer
newTimer rate = do
  current <- fromIntegral <$> SDL.ticks :: IO Word32
  return $ LoopTimer {
    rate = rate,
    current = current,
    elapsed = 0,
    lag = 0
  }

startLoop :: (MonadFail m, MonadIO m) => LoopTimer -> s -> Loop m s r -> m r
startLoop timer state loop = do
  (_, Right r) <- execStateT loop (timer, Left state)
  return r

timedLoop :: (MonadIO m) => InputHandler m e -> Updater m s r e -> Renderer m s -> Loop m s r
timedLoop inputHandler updater renderer = do
  updateTimer
  update inputHandler updater
  state <- gets snd
  case state of
    Left state' -> do
      lift $ renderer state'
      timedLoop inputHandler updater renderer
    Right result -> return ()

updateTimer :: (MonadIO m) => Loop m s r
updateTimer = do
  (timer, state) <- get
  newCurrent <- fromIntegral <$> SDL.ticks
  let elapsed = newCurrent - current timer
  put (timer { current = newCurrent, elapsed = elapsed, lag = lag timer + elapsed }, state)

update :: (MonadIO m) => InputHandler m e -> Updater m s r e -> Loop m s r
update inputHandler updater = do
  timedState <- get
  case timedState of
    (timer, Left state) ->
      when (lag timer > rate timer) $ do
        events <- lift inputHandler
        newState <- lift $ updater timer state events
        put (timer { lag = lag timer - rate timer}, newState)
        update inputHandler updater
    (_, Right _) -> return ()