module LambdaHeights.Table.Update where

import           Data.Maybe
import           LambdaHeights.Types.Table
import           Linear.V2
import qualified SDL

type ConvertEvent e = SDL.Event -> Maybe e
type ApplyEvent e = Table -> e -> Table
type LimitSelection = Table -> Table

data SelectEvent = SelectLeft | SelectUp | SelectRight | SelectDown

with :: ConvertEvent e -> ApplyEvent e -> UpdateTable
with convert apply events table = foldl apply table $ mapMaybe convert events

toSelectEvent :: ConvertEvent SelectEvent
toSelectEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let code   = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion = SDL.keyboardEventKeyMotion keyEvent
        fromKey SDL.KeycodeLeft  SDL.Pressed = Just SelectLeft
        fromKey SDL.KeycodeUp    SDL.Pressed = Just SelectUp
        fromKey SDL.KeycodeRight SDL.Pressed = Just SelectRight
        fromKey SDL.KeycodeDown  SDL.Pressed = Just SelectDown
        fromKey _                _           = Nothing
    in  fromKey code motion
  _ -> Nothing

applySelectEvent :: LimitSelection -> ApplyEvent SelectEvent
applySelectEvent limit table event =
  let V2 r c = selected table
  in  limit $ case event of
        SelectLeft  -> table { selected = V2 r (c - 1) }
        SelectUp    -> table { selected = V2 (r - 1) c }
        SelectRight -> table { selected = V2 r (c + 1) }
        SelectDown  -> table { selected = V2 (r + 1) c }

limitNotFirstRow :: LimitSelection -> LimitSelection
limitNotFirstRow parent table =
  let V2 r c = selected $ parent table
      r'     = if r < 2 then 2 else r
  in  table { selected = V2 r' c }

limitFirstColumn :: LimitSelection -> LimitSelection
limitFirstColumn parent table =
  let V2 r _ = selected $ parent table in table { selected = V2 r 1 }

limitAll :: LimitSelection
limitAll table =
  let V2 rm cm = tableDimension table
      V2 r  c  = selected table
      r' | r < 1     = 1
         | r > rm    = rm
         | otherwise = r
      c' | c < 1     = 1
         | c > cm    = cm
         | otherwise = c
  in  table { selected = V2 r' c' }
