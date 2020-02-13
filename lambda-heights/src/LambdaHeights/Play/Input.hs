module LambdaHeights.Play.Input
  ( keyInput,
  )
where

import Data.Maybe
import LambdaHeights.Types.Events
import qualified SDL

-- | Polls pending SDL events and maps them to Events.
keyInput :: IO Events
keyInput = do
  events <- SDL.pollEvents
  return $ Events (mapMaybe eventToControlEvent events) (mapMaybe eventToPlayerEvent events)

eventToControlEvent :: SDL.Event -> Maybe ControlEvent
eventToControlEvent event = case SDL.eventPayload event of
  SDL.QuitEvent -> Just Paused
  SDL.KeyboardEvent keyEvent -> keyToControlEvent $ keyEventProperties keyEvent
  _ -> Nothing

keyEventProperties :: SDL.KeyboardEventData -> (SDL.Keycode, SDL.InputMotion, Bool)
keyEventProperties keyEvent =
  let code = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
      motion = SDL.keyboardEventKeyMotion keyEvent
      repeat = SDL.keyboardEventRepeat keyEvent
   in (code, motion, repeat)

keyToControlEvent :: (SDL.Keycode, SDL.InputMotion, Bool) -> Maybe ControlEvent
keyToControlEvent (SDL.KeycodeEscape, SDL.Pressed, False) = Just Paused
keyToControlEvent (SDL.KeycodeP, SDL.Pressed, False) = Just Paused
keyToControlEvent (SDL.KeycodeMinus, SDL.Pressed, False) = Just Faster
keyToControlEvent (SDL.KeycodePlus, SDL.Pressed, False) = Just Slower
keyToControlEvent (_, _, _) = Nothing

eventToPlayerEvent :: SDL.Event -> Maybe PlayerEvent
eventToPlayerEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent -> keyToPlayerEvent $ keyEventProperties keyEvent
  _ -> Nothing

keyToPlayerEvent :: (SDL.Keycode, SDL.InputMotion, Bool) -> Maybe PlayerEvent
keyToPlayerEvent (SDL.KeycodeA, SDL.Pressed, _) = Just $ PlayerMoved MoveLeft True
keyToPlayerEvent (SDL.KeycodeA, SDL.Released, _) = Just $ PlayerMoved MoveLeft False
keyToPlayerEvent (SDL.KeycodeLeft, SDL.Pressed, _) = Just $ PlayerMoved MoveLeft True
keyToPlayerEvent (SDL.KeycodeLeft, SDL.Released, _) = Just $ PlayerMoved MoveLeft False
keyToPlayerEvent (SDL.KeycodeD, SDL.Pressed, _) = Just $ PlayerMoved MoveRight True
keyToPlayerEvent (SDL.KeycodeD, SDL.Released, _) = Just $ PlayerMoved MoveRight False
keyToPlayerEvent (SDL.KeycodeRight, SDL.Pressed, _) = Just $ PlayerMoved MoveRight True
keyToPlayerEvent (SDL.KeycodeRight, SDL.Released, _) = Just $ PlayerMoved MoveRight False
keyToPlayerEvent (SDL.KeycodeW, SDL.Pressed, False) = Just PlayerJumped
keyToPlayerEvent (SDL.KeycodeUp, SDL.Pressed, False) = Just PlayerJumped
keyToPlayerEvent (SDL.KeycodeSpace, SDL.Pressed, False) = Just PlayerJumped
keyToPlayerEvent (_, _, _) = Nothing
