module LambdaTower.Loop where

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
type Loop s r = StateT (TimedState s r) IO ()

type InputHandler e = IO e
type Updater s r e = LoopTimer -> s -> e -> IO (Either s r)
type Renderer s = s -> IO ()


newTimer :: Word32 -> IO LoopTimer
newTimer rate = do

  current <- fromIntegral <$> SDL.ticks :: IO Word32

  return $ LoopTimer {
    rate = rate,
    current = current,
    elapsed = 0,
    lag = 0
  }


startLoop :: LoopTimer -> s -> Loop s r -> IO r
startLoop timer state loop = do

  (_, Right r) <- execStateT loop (timer, Left state)
  return r


timedLoop :: InputHandler e -> Updater s r e -> Renderer s -> Loop s r
timedLoop inputHandler updater renderer = do

  updateTimer
  update inputHandler updater

  state <- gets snd
  case state of
    Left state' -> do
      liftIO $ renderer state'
      timedLoop inputHandler updater renderer
    Right result -> return ()


updateTimer :: Loop s r
updateTimer = do

  (timer, state) <- get
  newCurrent <- fromIntegral <$> SDL.ticks

  let elapsed = newCurrent - current timer
  put (timer { current = newCurrent, elapsed = elapsed, lag = lag timer + elapsed }, state)


update :: InputHandler e -> Updater s r e -> Loop s r
update inputHandler updater = do

  timedState <- get
  case timedState of
    (timer, Left state) ->
      when (lag timer > rate timer) $ do
        events <- liftIO inputHandler
        newState <- liftIO $ updater timer state events
        put (timer { lag = lag timer - rate timer}, newState)
        update inputHandler updater
    (_, Right _) -> return ()