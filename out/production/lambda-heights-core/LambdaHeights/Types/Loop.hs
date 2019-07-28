module LambdaHeights.Types.Loop where

import           Control.Monad.Reader
import           Control.Monad.State
import           LambdaHeights.Types.Timer

type UpdateState s r = State (LoopTimer, Either r s)
type OutputState m s r = ReaderT (LoopTimer, Either r s) m
type RenderState m s = ReaderT (LoopTimer, s) m

type Input m e = m e
type Update s r e = e -> UpdateState s r ()
type Output m s r e = e -> OutputState m s r ()
type Render m s = RenderState m s ()

getUpdateTimer :: UpdateState s r LoopTimer
getUpdateTimer = gets fst

getUpdateState :: UpdateState s r s
getUpdateState = do
  eitherState <- gets snd
  let Right state = eitherState
  return state

putUpdateTimer :: LoopTimer -> UpdateState s r ()
putUpdateTimer timer = do
  (_, eitherState) <- get
  put (timer, eitherState)

putUpdateState :: s -> UpdateState s r ()
putUpdateState state = do
  (timer, _) <- get
  put (timer, Right state)

putUpdateResult :: r -> UpdateState s r ()
putUpdateResult result = do
  (timer, _) <- get
  put (timer, Left result)

askOutputState :: (Monad m) => OutputState m s r (Either r s)
askOutputState = asks snd

askRenderTimer :: (Monad m) => RenderState m s LoopTimer
askRenderTimer = asks fst

askRenderState :: (Monad m) => RenderState m s s
askRenderState = asks snd
