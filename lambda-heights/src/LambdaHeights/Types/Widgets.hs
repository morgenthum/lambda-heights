module LambdaHeights.Types.Widgets where

import ComposeEngine.RenderContext
import Data.Word
import LambdaHeights.Render
import LambdaHeights.Vectors
import Linear.V4
import qualified SDL
import qualified SDL.Font as SDLF

type Color = V4 Word8

data Button
  = Button
      { buttonText :: String,
        buttonSelected :: Bool,
        buttonPos :: ScreenPosF Int,
        buttonSize :: ScreenSizeF Int,
        buttonRender :: RenderContext -> Button -> IO ()
      }

simpleButtonRenderer :: SDLF.Font -> (Color, Color) -> (Color, Color) -> RenderContext -> Button -> IO ()
simpleButtonRenderer font (fg, bg) (sfg, sbg) (window, renderer) button = do
  if buttonSelected button
    then SDL.rendererDrawColor renderer SDL.$= sbg
    else SDL.rendererDrawColor renderer SDL.$= bg
  let color = if buttonSelected button then sfg else fg
      SP pos = fromIntegral <$> buttonPos button
      SS size = fromIntegral <$> buttonSize button
      text = buttonText button
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P pos) size
  renderText renderer font color pos text
