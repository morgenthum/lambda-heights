module LambdaTower.Replay.Render where

import LambdaTower.Graphics
import LambdaTower.Loop

import qualified LambdaTower.Ingame.Render as IR
import qualified LambdaTower.Replay.ReplayState as RR

renderReplay :: Graphics -> IR.RenderConfig -> Renderer IO RR.ReplayState
renderReplay graphics config state = IR.defaultRender graphics config gameState
  where RR.ReplayState gameState _ = state