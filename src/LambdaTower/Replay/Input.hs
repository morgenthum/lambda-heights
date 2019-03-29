module LambdaTower.Replay.Input where

import           LambdaTower.Loop

import qualified LambdaTower.Ingame.Input      as Ingame
import qualified LambdaTower.Types.GameEvents  as Events

keyInput :: InputHandler IO [Events.ControlEvent]
keyInput = Events.controlEvents <$> Ingame.keyInput
