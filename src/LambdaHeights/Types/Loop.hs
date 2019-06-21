module LambdaHeights.Types.Loop where

import           Control.Monad.State
import           LambdaHeights.Types.Timer

type UpdateState s r = State (LoopTimer, Either r s)

type Input m e = m e
type Update s r e = e -> UpdateState s r ()
type Output m s r e = LoopTimer -> e -> Either r s -> m ()
type Render m s = LoopTimer -> s -> m ()

getTimer :: UpdateState s r LoopTimer
getTimer = gets fst

getState :: UpdateState s r s
getState = do
  eitherState <- gets snd
  let Right state = eitherState
  return state

putTimer :: LoopTimer -> UpdateState s r ()
putTimer timer = do
  (_, eitherState) <- get
  put (timer, eitherState)

putState :: s -> UpdateState s r ()
putState state = do
  (timer, _) <- get
  put (timer, Right state)

putResult :: r -> UpdateState s r ()
putResult result = do
  (timer, _) <- get
  put (timer, Left result)
