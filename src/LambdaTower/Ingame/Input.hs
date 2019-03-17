module LambdaTower.Ingame.Input (
  dummyHandleInput,
  handleKeyInput
) where

import Data.Maybe

import qualified SDL

import LambdaTower.Ingame.Events
import LambdaTower.Loop

dummyHandleInput :: a -> InputHandler IO a
dummyHandleInput x = SDL.pollEvents >> return x

handleKeyInput :: InputHandler IO [PlayerEvent]
handleKeyInput = mapMaybe eventToPlayerEvent <$> SDL.pollEvents

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
keyToPlayerEvent SDL.KeycodeLeft  SDL.Pressed  = Just $ PlayerMoved MoveLeft True
keyToPlayerEvent SDL.KeycodeLeft  SDL.Released = Just $ PlayerMoved MoveLeft False
keyToPlayerEvent SDL.KeycodeD     SDL.Pressed  = Just $ PlayerMoved MoveRight True
keyToPlayerEvent SDL.KeycodeD     SDL.Released = Just $ PlayerMoved MoveRight False
keyToPlayerEvent SDL.KeycodeRight SDL.Pressed  = Just $ PlayerMoved MoveRight True
keyToPlayerEvent SDL.KeycodeRight SDL.Released = Just $ PlayerMoved MoveRight False
keyToPlayerEvent SDL.KeycodeSpace SDL.Pressed  = Just PlayerJumped
keyToPlayerEvent _                _            = Nothing