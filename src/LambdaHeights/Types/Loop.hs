module LambdaHeights.Types.Loop where

import LambdaHeights.Types.Timer

type Input m e = m e
type Update s r e = LoopTimer -> e -> s -> (LoopTimer, Either r s)
type Output m s r e = LoopTimer -> e -> Either r s -> m ()
type Render m s = LoopTimer -> s -> m ()