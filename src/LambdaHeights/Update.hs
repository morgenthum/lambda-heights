module LambdaHeights.Update where

import           Control.Monad.State
import           LambdaHeights.Types.Loop

updateOneFinished
  :: Update s1 r1 e1 -> Update s2 r2 e2 -> Update (s1, s2) (Maybe r1, Maybe r2) (e1, e2)
updateOneFinished u1 u2 (e1, e2) = do
  timer    <- getTimer
  (s1, s2) <- getState
  let (timer', updated1)  = execState (u1 e1) (timer, Right s1)
  let (timer'', updated2) = execState (u2 e2) (timer', Right s2)
  putTimer timer''
  case updated1 of
    Left r1' -> case updated2 of
      Left  r2' -> putResult (Just r1', Just r2')
      Right _   -> putResult (Just r1', Nothing)
    Right s1' -> case updated2 of
      Left  r2' -> putResult (Nothing, Just r2')
      Right s2' -> putState (s1', s2')
