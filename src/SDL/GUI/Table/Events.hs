module SDL.GUI.Table.Events where

import Linear.V2
import SDL.GUI.Table.Types
import SDL.GUI.Events.Types

applySelects :: Table -> [SelectEvent] -> Table
applySelects = foldl applySelect

applySelect :: Table -> SelectEvent -> Table
applySelect table SelectLeft = move table $ V2 0 (-1)
applySelect table SelectUp = move table $ V2 (-1) 0
applySelect table SelectRight = move table $ V2 0 1
applySelect table SelectDown = move table $ V2 1 0

move :: Table -> V2 Int -> Table
move table direction = table { selected = selected table + direction }
