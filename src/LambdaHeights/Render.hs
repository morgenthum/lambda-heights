module LambdaHeights.Render where

import qualified Data.Text                  as T
import           LambdaHeights.GUI.Basics
import qualified LambdaHeights.Scale        as Scale
import qualified LambdaHeights.Types.Label  as Label
import qualified LambdaHeights.Types.Screen as Screen
import           Linear.V2
import qualified SDL
import qualified SDL.Font                   as SDLF

renderLabel
  :: SDL.Renderer
  -> Scale.WindowSize
  -> Screen.Screen
  -> SDLF.Font
  -> SDLF.Color
  -> Label.Label
  -> IO ()
renderLabel renderer windowSize screen font color button = do
  let text     = Label.text button
  let (V2 x y) = Scale.toWindowPosition screen windowSize (Label.position button)
  case Label.alignment button of
    Label.AlignLeft   -> renderText renderer font (V2 x y) color text
    Label.AlignCenter -> do
      (w, h) <- SDLF.size font $ T.pack text
      let deltaX   = round (realToFrac w / 2 :: Float)
      let deltaY   = round (realToFrac h / 2 :: Float)
      let position = V2 (x - deltaX) (y - deltaY)
      renderText renderer font position color text
