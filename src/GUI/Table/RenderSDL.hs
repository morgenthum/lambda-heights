module GUI.Table.RenderSDL where

import           Data.Matrix
import qualified Data.Text        as T
import           Data.Word
import           GUI.Table.Render
import           GUI.Table.Types
import           GUI.TextRenderer
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font         as SDLF

type Color = V4 Word8

data CellStyle = CellStyle {
  cellFont :: SDLF.Font,
  cellBg   :: Color,
  cellFg   :: Color
}

loadFontSizes :: SDLF.Font -> DataMatrix -> IO SizeMatrix
loadFontSizes font m = do
  sm <- mapM (SDLF.size font . T.pack) m
  return $ fmap (uncurry V2) sm

styleSimple :: (CellStyle, CellStyle, CellStyle) -> StyleGenerator CellStyle
styleSimple (headStyle, _, _) _ (1, _) = headStyle
styleSimple (_, selectedStyle, defaultStyle) table (r, _) =
  let V2 sr _ = selected table in if sr == r then selectedStyle else defaultStyle

renderSimpleCell :: SDL.Renderer -> RenderCell CellStyle
renderSimpleCell renderer table styles sizes positions (V2 r c) = do
  let text   = getElem r c $ content table
  let style  = getElem r c styles
  let size   = convertV2 $ getElem r c sizes
  let V2 x y = convertV2 $ getElem r c positions
  SDL.rendererDrawColor renderer SDL.$= cellBg style
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ V2 x y) size
  renderText renderer (cellFont style) (convertV2 $ V2 (x + 10) (y + 10)) (cellFg style) text
  return ()

convertV2 :: (Integral a, Integral b) => V2 a -> V2 b
convertV2 (V2 x y) = V2 (fromIntegral x) (fromIntegral y)
