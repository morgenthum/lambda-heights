module LambdaTower.Replay.Render where

import           LambdaTower.Graphics

import qualified LambdaTower.Ingame.Render     as Ingame
import qualified LambdaTower.Replay.State      as Replay
import qualified LambdaTower.Timing.Timer      as Timer

render :: Graphics -> Ingame.RenderConfig -> Timer.LoopTimer -> Replay.State -> IO ()
render graphics config timer = Ingame.renderDefault graphics config timer . Replay.state
