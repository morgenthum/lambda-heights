module LambdaHeights.Play.Render
  ( RenderConfig(..)
  , createConfig
  , deleteConfig
  , renderDefault
  , renderPause
  , render
  , clear
  )
where

import qualified Data.Vector.Storable          as V
import           Data.Word
import           Foreign.C.Types
import qualified LambdaHeights.Render          as Render
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Scale           as Scale
import           LambdaHeights.Types
import qualified LambdaHeights.Types.Layer     as Layer
import qualified LambdaHeights.Types.Player    as Player
import qualified LambdaHeights.Types.PlayState as State
import qualified LambdaHeights.Types.Screen    as Screen
import qualified LambdaHeights.Types.Timer     as Timer
import           Linear.V2
import qualified SDL
import qualified SDL.Font                      as SDLF
import qualified SDL.Primitive                 as SDLP

data RenderConfig = RenderConfig {
  font              :: SDLF.Font,
  headlineColor     :: SDL.V4 Word8,
  bgColor           :: SDL.V4 Word8,
  playerColor       :: SDL.V4 Word8,
  playerShadowColor :: SDL.V4 Word8,
  textColor         :: SDL.V4 Word8
}

createConfig :: IO RenderConfig
createConfig = do
  loadedFont <- SDLF.load "fonts/retro_gaming.ttf" 11
  return $ RenderConfig
    { font              = loadedFont
    , headlineColor     = SDL.V4 255 255 255 255
    , bgColor           = SDL.V4 30 30 30 255
    , playerColor       = SDL.V4 135 31 120 255
    , playerShadowColor = SDL.V4 255 255 255 255
    , textColor         = SDL.V4 0 191 255 255
    }

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font

-- | Clears the screen, renders the state and presents the screen.
renderDefault :: RenderContext -> RenderConfig -> Timer.LoopTimer -> State.State -> IO ()
renderDefault (window, renderer) config =
  render (clear renderer $ bgColor config) (SDL.present renderer) (window, renderer) config

-- | Clears the screen, renders the state but does not presents it.
renderPause :: RenderContext -> RenderConfig -> Timer.LoopTimer -> State.State -> IO ()
renderPause (window, renderer) config =
  render (clear renderer $ bgColor config) (return ()) (window, renderer) config

-- | Renders the state and executes an pre and post action.
render :: IO () -> IO () -> RenderContext -> RenderConfig -> Timer.LoopTimer -> State.State -> IO ()
render pre post (window, renderer) config timer state = do
  pre
  windowSize <- SDL.get $ SDL.windowSize window
  mapM_ (renderLayer renderer windowSize $ State.screen state) $ State.layers state
  renderPlayer renderer config windowSize (State.screen state) (State.player state)
  renderPlayerShadow renderer config windowSize (State.screen state) (State.player state)
  renderHud renderer config windowSize timer state
  post

-- | Clears the screen with given color.
clear :: SDL.Renderer -> SDLP.Color -> IO ()
clear renderer color = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer

renderHud :: SDL.Renderer -> RenderConfig -> V2 CInt -> Timer.LoopTimer -> State.State -> IO ()
renderHud renderer config windowSize timer state = do
  renderHudVelocity renderer config state
  renderHudTime renderer config state
  renderHudScore renderer config state
  renderHudFPS renderer config windowSize timer

renderHudVelocity :: SDL.Renderer -> RenderConfig -> State.State -> IO ()
renderHudVelocity renderer config state = do
  let textFont = font config
  let V2 x y   = Player.velocity . State.player $ state
  Render.renderText renderer textFont (headlineColor config) (V2 20 20) "VELOCITY"
  Render.renderText renderer textFont (textColor config) (V2 100 20) $ show (round x :: Int)
  Render.renderText renderer textFont (textColor config) (V2 150 20) $ show (round y :: Int)

renderHudTime :: SDL.Renderer -> RenderConfig -> State.State -> IO ()
renderHudTime renderer config state = do
  let textFont = font config
  let duration = State.duration state
  let seconds = truncate (realToFrac duration / 1000 :: Float) :: Integer
  let millis   = mod duration 1000
  Render.renderText renderer textFont (headlineColor config) (V2 20 40) "TIME"
  Render.renderText renderer textFont (textColor config) (V2 100 40) (show seconds)
  Render.renderText renderer textFont (textColor config) (V2 150 40) (show millis)

renderHudScore :: SDL.Renderer -> RenderConfig -> State.State -> IO ()
renderHudScore renderer config state = do
  let textFont = font config
  let score    = Player.score . State.player $ state
  Render.renderText renderer textFont (headlineColor config) (V2 20 60) "SCORE"
  Render.renderText renderer textFont (textColor config) (V2 100 60) (show score)

renderHudFPS :: SDL.Renderer -> RenderConfig -> V2 CInt -> Timer.LoopTimer -> IO ()
renderHudFPS renderer config (V2 w _) timer = do
  let textFont = font config
  let fps      = Timer.fps . Timer.counter $ timer
  let x        = w - 125
  Render.renderText renderer textFont (headlineColor config) (V2 x 20) "FPS"
  Render.renderText renderer textFont (textColor config) (V2 (x + 70) 20) (show fps)

data Shape = Shape [Float] [Float]

shapeSize :: Shape -> Size
shapeSize (Shape xs ys) = V2 (maximum xs) (maximum ys)

renderPlayer :: SDL.Renderer -> RenderConfig -> V2 CInt -> Screen.Screen -> Player.Player -> IO ()
renderPlayer renderer config windowSize screen player = do
  let shape = Shape [0, 10, 40, 30, 20, 10, 0, 15] [80, 80, 0, 0, 25, 0, 0, 40]
  let translatedShape =
        centerBottom (Player.position player) $ flipByVel (Player.velocity player) shape
  renderShape renderer windowSize screen (playerColor config) translatedShape

renderPlayerShadow
  :: SDL.Renderer -> RenderConfig -> V2 CInt -> Screen.Screen -> Player.Player -> IO ()
renderPlayerShadow renderer config windowSize screen player = do
  let V2 posX posY = Player.position player
  let V2 velX _    = Player.velocity player
  let offX         = if velX >= 0 then -20 else 20
  let shape        = Shape [0, 10, 25, 10, 0, 15] [80, 80, 40, 0, 0, 40]
  let translatedShape =
        centerBottom (V2 (posX + offX) posY) $ flipByVel (Player.velocity player) shape
  let SDL.V4 r g b _ = playerShadowColor config
  let a = round $ if abs velX > 10000 then 255 else abs velX / 10000 * 255
  renderShape renderer windowSize screen (SDL.V4 r g b a) translatedShape

renderShape :: SDL.Renderer -> V2 CInt -> Screen.Screen -> SDLP.Color -> Shape -> IO ()
renderShape renderer (V2 w h) screen color (Shape xs ys) = do
  let toVector   = foldl V.snoc V.empty
  let transformX = fromIntegral . Scale.translate screen w
  let transformY = fromIntegral . Scale.translateFlipped screen h
  let xs'        = toVector . map transformX $ xs
  let ys'        = toVector . map transformY $ ys
  SDLP.fillPolygon renderer xs' ys' color

renderLayer :: SDL.Renderer -> V2 CInt -> Screen.Screen -> Layer.Layer -> IO ()
renderLayer renderer windowSize screen layer = do
  let size     = Scale.toWindowSize screen windowSize (Layer.size layer)
  let position = Scale.toWindowPosition screen windowSize (Layer.position layer)
  SDL.rendererDrawColor renderer SDL.$= layerColor layer
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P position) size

flipByVel :: Player.Velocity -> Shape -> Shape
flipByVel (V2 velX _) shape = if velX >= 0 then shape else flipShape shape

flipShape :: Shape -> Shape
flipShape (Shape xs ys) = let V2 w _ = shapeSize (Shape xs ys) in Shape (map (\x -> w - x) xs) ys

centerBottom :: Position -> Shape -> Shape
centerBottom (V2 posX posY) (Shape xs ys) =
  let V2 w _ = shapeSize (Shape xs ys)
      xs'    = map ((\x -> x - w / 2) . (+ posX)) xs
      ys'    = map (+ posY) ys
  in  Shape xs' ys'

layerColor :: Layer.Layer -> SDL.V4 Word8
layerColor layer =
  let V2 w h = Layer.size layer
      go | h >= 300  = SDL.V4 0 255 127 255
         | w >= 1000 = SDL.V4 255 255 255 255
         | w >= 500  = SDL.V4 75 0 130 255
         | w >= 400  = SDL.V4 31 135 120 255
         | w >= 300  = SDL.V4 255 127 80 255
         | otherwise = SDL.V4 255 215 0 255
  in  go
