module LambdaTower.Replay.Input where

import qualified LambdaTower.Ingame.Events     as Ingame
import qualified LambdaTower.Ingame.Input      as Ingame

keyInput :: IO [Ingame.ControlEvent]
keyInput = Ingame.controlEvents <$> Ingame.keyInput
