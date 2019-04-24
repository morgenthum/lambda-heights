module SDL.GUI.Types.Table where

import           Data.Matrix
import Foreign.C.Types
import Linear.V2
import qualified SDL.Font             as SDLF

data TableEntry = TableEntry {
  entryPos :: V2 Int,
  entryText :: String
}

data Alignment = AlignLeft | AlignCenter | AlignRight

data TableStyle = TableStyle {
  alignment  :: Alignment,
  columnGap  :: Int,
  rowGap     :: Int,
  rowHeight  :: Int,
  maxRows    :: Int
}

data CellStyle = CellStyle {
  background :: SDLF.Color,
  foreground :: SDLF.Color,
  font :: SDLF.Font
}

data RenderCell = RenderCell {
  text :: String,
  style :: CellStyle,
  position :: V2 CInt,
  size :: V2 CInt
}

newHeader :: [String] -> Matrix String
newHeader xs = fromList 1 (length xs) xs

newContent :: [[String]] -> Matrix String
newContent = fromLists
