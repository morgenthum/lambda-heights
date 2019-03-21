module LambdaTower.Ingame.Input (
  ingameKeyInput
) where

import Data.Maybe

import qualified SDL

import LambdaTower.Loop

import qualified LambdaTower.Ingame.GameEvents as IE

ingameKeyInput :: InputHandler IO IE.GameEvents
ingameKeyInput = do
  events <- SDL.pollEvents
  let controlEvents = mapMaybe eventToControlEvent events
  let playerEvents = mapMaybe eventToPlayerEvent events
  return $ IE.GameEvents controlEvents playerEvents

eventToControlEvent :: SDL.Event -> Maybe IE.ControlEvent
eventToControlEvent event =
  case SDL.eventPayload event of
    SDL.QuitEvent -> Just IE.Paused
    SDL.KeyboardEvent keyEvent -> keyToControlEvent code motion
      where code = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
            motion = SDL.keyboardEventKeyMotion keyEvent
    _ -> Nothing

keyToControlEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe IE.ControlEvent
keyToControlEvent SDL.KeycodeEscape SDL.Pressed  = Just IE.Paused
keyToControlEvent SDL.KeycodeP      SDL.Pressed  = Just IE.Paused
keyToControlEvent _                 _            = Nothing

eventToPlayerEvent :: SDL.Event -> Maybe IE.PlayerEvent
eventToPlayerEvent event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyEvent -> keyToPlayerEvent code motion
      where code = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
            motion = SDL.keyboardEventKeyMotion keyEvent
    _ -> Nothing

keyToPlayerEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe IE.PlayerEvent
keyToPlayerEvent SDL.KeycodeA      SDL.Pressed  = Just $ IE.PlayerMoved IE.MoveLeft True
keyToPlayerEvent SDL.KeycodeA      SDL.Released = Just $ IE.PlayerMoved IE.MoveLeft False
keyToPlayerEvent SDL.KeycodeLeft   SDL.Pressed  = Just $ IE.PlayerMoved IE.MoveLeft True
keyToPlayerEvent SDL.KeycodeLeft   SDL.Released = Just $ IE.PlayerMoved IE.MoveLeft False
keyToPlayerEvent SDL.KeycodeD      SDL.Pressed  = Just $ IE.PlayerMoved IE.MoveRight True
keyToPlayerEvent SDL.KeycodeD      SDL.Released = Just $ IE.PlayerMoved IE.MoveRight False
keyToPlayerEvent SDL.KeycodeRight  SDL.Pressed  = Just $ IE.PlayerMoved IE.MoveRight True
keyToPlayerEvent SDL.KeycodeRight  SDL.Released = Just $ IE.PlayerMoved IE.MoveRight False
keyToPlayerEvent SDL.KeycodeSpace  SDL.Pressed  = Just IE.PlayerJumped
keyToPlayerEvent _                 _            = Nothing