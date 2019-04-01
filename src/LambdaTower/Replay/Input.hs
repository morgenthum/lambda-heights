module LambdaTower.Replay.Input where

import qualified LambdaTower.Ingame.GameEvents as Events
import qualified LambdaTower.Ingame.Input      as Ingame

keyInput :: IO [Events.ControlEvent]
keyInput = Events.controlEvents <$> Ingame.keyInput
