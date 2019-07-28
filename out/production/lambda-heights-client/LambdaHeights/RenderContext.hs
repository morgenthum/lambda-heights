module LambdaHeights.RenderContext
  ( RenderContext
  , createContext
  , deleteContext
  )
where

import           Data.Text
import qualified SDL

type RenderContext = (SDL.Window, SDL.Renderer)

-- | Initializes SDL and creates required components.
createContext :: Text -> IO RenderContext
createContext windowTitle = do
  let windowSettings = SDL.defaultWindow { SDL.windowMode = SDL.FullscreenDesktop }
  window   <- SDL.createWindow windowTitle windowSettings
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  return (window, renderer)

-- | Destroys components and uninitializes SDL.
deleteContext :: RenderContext -> IO ()
deleteContext (window, renderer) = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
