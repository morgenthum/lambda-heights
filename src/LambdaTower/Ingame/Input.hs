module LambdaTower.Ingame.Input (
  replayKeyInput,
  ingameKeyInput
) where

import Data.Maybe

import qualified SDL

import LambdaTower.Ingame.GameEvents
import LambdaTower.Loop

replayKeyInput ::  InputHandler IO [ControlEvent]
replayKeyInput = do
  gameEvents <- ingameKeyInput
  case gameEvents of
    GameEvents controlEvents _ -> return controlEvents

ingameKeyInput :: InputHandler IO GameEvents
ingameKeyInput = do
  events <- SDL.pollEvents
  let controlEvents = mapMaybe eventToControlEvent events
  let playerEvents = mapMaybe eventToPlayerEvent events
  return $ GameEvents controlEvents playerEvents

eventToControlEvent :: SDL.Event -> Maybe ControlEvent
eventToControlEvent event =
  case SDL.eventPayload event of
    SDL.QuitEvent -> Just Paused
    SDL.KeyboardEvent keyEvent -> keyToControlEvent code motion
      where code = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
            motion = SDL.keyboardEventKeyMotion keyEvent
    _ -> Nothing

keyToControlEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe ControlEvent
keyToControlEvent SDL.KeycodeEscape SDL.Pressed  = Just Paused
keyToControlEvent _                 _            = Nothing

eventToPlayerEvent :: SDL.Event -> Maybe PlayerEvent
eventToPlayerEvent event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyEvent -> keyToPlayerEvent code motion
      where code = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
            motion = SDL.keyboardEventKeyMotion keyEvent
    _ -> Nothing

keyToPlayerEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe PlayerEvent
keyToPlayerEvent SDL.KeycodeA      SDL.Pressed  = Just $ PlayerMoved MoveLeft True
keyToPlayerEvent SDL.KeycodeA      SDL.Released = Just $ PlayerMoved MoveLeft False
keyToPlayerEvent SDL.KeycodeLeft   SDL.Pressed  = Just $ PlayerMoved MoveLeft True
keyToPlayerEvent SDL.KeycodeLeft   SDL.Released = Just $ PlayerMoved MoveLeft False
keyToPlayerEvent SDL.KeycodeD      SDL.Pressed  = Just $ PlayerMoved MoveRight True
keyToPlayerEvent SDL.KeycodeD      SDL.Released = Just $ PlayerMoved MoveRight False
keyToPlayerEvent SDL.KeycodeRight  SDL.Pressed  = Just $ PlayerMoved MoveRight True
keyToPlayerEvent SDL.KeycodeRight  SDL.Released = Just $ PlayerMoved MoveRight False
keyToPlayerEvent SDL.KeycodeSpace  SDL.Pressed  = Just PlayerJumped
keyToPlayerEvent _                 _            = Nothing