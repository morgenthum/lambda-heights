module SDL.GUI.TableRenderer where

import qualified Data.Matrix          as M
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Linear.V2
import qualified SDL
import qualified SDL.Font             as SDLF
import           SDL.GUI.TextRenderer
import           SDL.GUI.Types.Table

type SizeMatrix = M.Matrix (V2 Int)

render :: SDL.Renderer -> IO ()
render renderer = do
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
  SDL.clear renderer
  SDL.present renderer

fromV2 :: (Integral a, Num b) => V2 a -> V2 b
fromV2 (V2 x y) = V2 (fromIntegral x) (fromIntegral y)
