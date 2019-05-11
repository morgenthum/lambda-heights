module LambdaHeights.GUI.Table.CellSizers where

import           Data.Matrix
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           LambdaHeights.GUI.Table.Types
import           Linear.V2
import qualified SDL.Font                      as SDLF

type SizeGen = Table -> (Int, Int) -> Size

loadFontSizes :: SDLF.Font -> Matrix String -> IO (Matrix Size)
loadFontSizes font m = do
  sm <- mapM (SDLF.size font . T.pack) m
  return $ fmap (uncurry V2) sm

alignWidths :: CellSizer a -> CellSizer a
alignWidths parent table styles =
  let sizes = parent table styles
  in  mapPos (\(_, c) (V2 _ h) -> V2 (maxColumnWidth c sizes) h) sizes

with :: SizeGen -> CellSizer a
with generator table _ = let V2 r c = tableDimension table in matrix r c $ generator table

fixedSize :: Size -> SizeGen
fixedSize size _ _ = size

copy :: Matrix Size -> SizeGen
copy sizes _ (r, c) = getElem r c sizes

extend :: Size -> SizeGen -> SizeGen
extend size parent table (r, c) = size + parent table (r, c)

maxColumnWidth :: Int -> Matrix Size -> Int
maxColumnWidth c sizes =
  let cs = getCol c sizes
      maxWidth w1 (V2 w2 _) = max w1 w2
  in  V.foldl maxWidth 0 cs
