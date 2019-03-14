module LambdaTower.Ingame.Input (
  dummyInputHandler,
  keyInputHandler
) where

import Data.Maybe

import qualified SDL

import LambdaTower.Ingame.Events
import LambdaTower.Loop

dummyInputHandler :: InputHandler IO ()
dummyInputHandler = return ()

keyInputHandler :: InputHandler IO [PlayerEvent]
keyInputHandler = mapMaybe eventToPlayerEvent <$> SDL.pollEvents

eventToPlayerEvent :: SDL.Event -> Maybe PlayerEvent
eventToPlayerEvent event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyEvent -> keyToPlayerEvent code motion
      where code = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
            motion = SDL.keyboardEventKeyMotion keyEvent
    _ -> Nothing

keyToPlayerEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe PlayerEvent
keyToPlayerEvent SDL.KeycodeA     SDL.Pressed  = Just $ PlayerMoved MoveLeft True
keyToPlayerEvent SDL.KeycodeA     SDL.Released = Just $ PlayerMoved MoveLeft False
keyToPlayerEvent SDL.KeycodeD     SDL.Pressed  = Just $ PlayerMoved MoveRight True
keyToPlayerEvent SDL.KeycodeD     SDL.Released = Just $ PlayerMoved MoveRight False
keyToPlayerEvent SDL.KeycodeSpace SDL.Pressed  = Just PlayerJumped
keyToPlayerEvent _                _            = Nothing