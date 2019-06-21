module LambdaHeights.Update where

import           LambdaHeights.Loop

updateOneFinished
  :: Update s1 r1 e1 -> Update s2 r2 e2 -> Update (s1, s2) (Maybe r1, Maybe r2) (e1, e2)
updateOneFinished u1 u2 timer (e1, e2) (s1, s2) =
  let (timer', updated1) = u1 timer e1 s1
      (timer'', updated2) = u2 timer' e2 s2
  in  case updated1 of
        Left r1' -> case updated2 of
          Left  r2' -> (timer'', Left (Just r1', Just r2'))
          Right _   -> (timer'', Left (Just r1', Nothing))
        Right s1' -> case updated2 of
          Left  r2' -> (timer'', Left (Nothing, Just r2'))
          Right s2' -> (timer'', Right (s1', s2'))
