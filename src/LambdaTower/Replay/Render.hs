module LambdaTower.Replay.Render where

import           LambdaTower.Graphics
import           LambdaTower.Loop

import qualified LambdaTower.Ingame.Render     as Ingame
import qualified LambdaTower.Types.ReplayState as State

render :: Graphics -> Ingame.RenderConfig -> Renderer IO State.ReplayState
render graphics config timer = Ingame.renderDefault graphics config timer . State.state
