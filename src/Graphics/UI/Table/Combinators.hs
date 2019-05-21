module Graphics.UI.Table.Combinators where

import           Data.Matrix
import           Data.Maybe
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           Graphics.UI.Types
import           Graphics.UI.Types.Table
import           Linear.V2
import qualified SDL.Font                as SDLF

type Selector = Table -> (Int, Int) -> Bool
type StyleGen a = Table -> (Int, Int) -> a
type SizeGen = Table -> (Int, Int) -> Size
type LocationGen = Table -> (Int, Int) -> Position
type TextLocationGen = (Int, Int) -> Position

merge
  :: Table
  -> Matrix CellStyle
  -> Matrix Size
  -> Matrix Position
  -> Matrix Position
  -> Matrix CellView
merge table styles sizes positions textPositions =
  let V2 rCount cCount = tableDimension table
      f (r, c) = CellView (getElem r c $ content table)
                          (getElem r c styles)
                          (getElem r c sizes)
                          (getElem r c positions)
                          (getElem r c textPositions)
  in  matrix rCount cCount f

-- Selectors

selectedRow :: Selector
selectedRow table (r, _) = let V2 sr _ = selected table in sr == r

-- Style combinators

styleWith :: StyleGen CellStyle -> CellStyler
styleWith g table = let V2 r c = tableDimension table in matrix r c $ g table

prefer :: StyleGen (Maybe a) -> StyleGen a -> StyleGen a
prefer preferred standard table pos = fromMaybe (standard table pos) (preferred table pos)

header :: a -> StyleGen (Maybe a)
header headStyle _ (1, _) = Just headStyle
header _         _ _      = Nothing

selectedAndBody :: a -> a -> StyleGen a
selectedAndBody selectedStyle bodyStyle table (r, _) =
  let V2 sr _ = selected table in if sr == r then selectedStyle else bodyStyle

-- Size combinators

loadFontSizes :: SDLF.Font -> Matrix String -> IO (Matrix Size)
loadFontSizes font m = do
  sm <- mapM (SDLF.size font . T.pack) m
  return $ fmap (uncurry V2) sm

alignWidths :: CellSizer -> CellSizer
alignWidths parent table =
  let sm = parent table in mapPos (\(_, c) (V2 _ h) -> V2 (maxColumnWidth c sm) h) sm

sizeWith :: SizeGen -> CellSizer
sizeWith generator table = let V2 r c = tableDimension table in matrix r c $ generator table

fixedSize :: Size -> SizeGen
fixedSize size _ _ = size

copy :: Matrix Size -> SizeGen
copy sm _ (r, c) = getElem r c sm

extend :: Size -> SizeGen -> SizeGen
extend size parent table (r, c) = size + parent table (r, c)

maxColumnWidth :: Int -> Matrix Size -> Int
maxColumnWidth c sm =
  let cs = getCol c sm
      maxWidth w1 (V2 w2 _) = max w1 w2
  in  V.foldl maxWidth 0 cs

-- Position combinators

positionWith :: LocationGen -> CellPositioner
positionWith g table = let V2 r c = tableDimension table in matrix r c $ g table

grid :: Matrix Size -> LocationGen
grid sm _ (r, c) =
  let cs = [1 .. c - 1]
      rs = [1 .. r - 1]
      ws = map ((\(V2 w _) -> w) . (\c' -> getElem r c' sm)) cs
      hs = map ((\(V2 _ h) -> h) . (\r' -> getElem r' c sm)) rs
  in  V2 (sum ws) (sum hs)

addGaps :: Size -> LocationGen -> LocationGen
addGaps (V2 xGap yGap) g table (r, c) =
  let V2 x y = g table (r, c)
      x'     = x + xGap * (c - 1)
      y'     = y + yGap * (r - 1)
  in  V2 x' y'

indent :: Selector -> V2 Int -> LocationGen -> LocationGen
indent s (V2 ix iy) g table (r, c) =
  let V2 x y = g table (r, c) in if s table (r, c) then V2 (x + ix) (y + iy) else V2 x y

move :: V2 Int -> LocationGen -> LocationGen
move pos g table loc = g table loc + pos

-- Text position combinators

positionTextWith :: TextLocationGen -> TextPositioner
positionTextWith g table = let V2 r c = tableDimension table in matrix r c g

indentText :: Matrix Position -> V2 Int -> TextLocationGen
indentText pm (V2 ix iy) (r, c) = let V2 x y = getElem r c pm in V2 (x + ix) (y + iy)

centerText :: Matrix Size -> Matrix Position -> Matrix Size -> TextLocationGen
centerText sm pm fontSizes (r, c) =
  let V2 fw fh = getElem r c fontSizes :: V2 Int
      V2 w  h  = getElem r c sm :: V2 Int
      V2 x  y  = getElem r c pm
      x'       = x + round (realToFrac w / 2 :: Float) - round (realToFrac fw / 2 :: Float)
      y'       = y + round (realToFrac h / 2 :: Float) - round (realToFrac fh / 2 :: Float)
  in  V2 x' y'
