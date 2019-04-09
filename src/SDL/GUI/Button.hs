module SDL.GUI.Button where

import           Data.Word

import           SDL.GUI.Renderable
import           SDL.GUI.Types

import qualified Data.Text                               as T

import qualified SDL
import qualified SDL.Font                                as SDLF

import qualified SDL.GUI.Render                          as Render

data Style = Style {
  font :: SDLF.Font,
  foreground :: SDL.V4 Word8,
  background :: SDL.V4 Word8
}

data Button = Button {
  location :: Location,
  style :: Style,
  text :: String
}

instance Renderable Button where
  render ctx button = do
    renderBackground ctx button
    renderForeground ctx button

renderBackground :: RenderContext -> Button -> IO ()
renderBackground ctx button = do
  let rect = SDL.Rectangle (position $ location button) (size $ location button)
  SDL.rendererDrawColor (renderer ctx) SDL.$= (background $ style button)
  SDL.fillRect (renderer ctx) $ Just rect

renderForeground :: RenderContext -> Button -> IO ()
renderForeground ctx button = do
  let f                  = font $ style button
  let fg                 = foreground $ style button
  let SDL.P (SDL.V2 x y) = position $ location button
  let SDL.V2 w h         = size $ location button
  (tw, th) <- SDLF.size f $ T.pack $ text button
  let xMargin = round $ ((fromIntegral w - fromIntegral tw) / 2 :: Float)
  let yMargin = round $ ((fromIntegral h - fromIntegral th) / 2 :: Float)
  let p       = SDL.V2 (x + xMargin) (y + yMargin)
  Render.renderText (renderer ctx) f p fg (text button)
