module SDL.GUI.Table.CellSizers where

import           Data.Matrix
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Linear.V2
import qualified SDL.Font            as SDLF
import           SDL.GUI.Table.Types

loadFontSizes :: SDLF.Font -> DataMatrix -> IO SizeMatrix
loadFontSizes font m = do
  sm <- mapM (SDLF.size font . T.pack) m
  return $ fmap (uncurry V2) sm

alignWidths :: SizeCells a -> SizeCells a
alignWidths parent table styles = do
  sizes <- parent table styles
  return $ mapPos (\(_, c) (V2 _ h) -> V2 (maxColumnWidth c sizes) h) sizes

with :: SizeGenerator a -> SizeCells a
with generator table _ = do
  let (rCount, cCount) = tableDimension table
  return $ matrix rCount cCount $ generator table

fixedSize :: Size -> SizeGenerator a
fixedSize size _ _ = size

deriveFrom :: SizeMatrix -> SizeGenerator a
deriveFrom sizes _ (r, c) = getElem r c sizes

extend :: Size -> SizeGenerator a -> SizeGenerator a
extend size parent table (r, c) = size + parent table (r, c)

maxColumnWidth :: Int -> SizeMatrix -> Int
maxColumnWidth c sizes =
  let cs = getCol c sizes
      maxWidth w1 (V2 w2 _) = max w1 w2
  in  V.foldl maxWidth 0 cs
