module LambdaHeights.Play.Render
  ( RenderConfig(..)
  , createConfig
  , render
  , clear
  )
where

import qualified Control.Monad.IO.Class        as M
import qualified Control.Monad.Reader          as M
import qualified Data.Vector.Storable          as V
import           Data.Word
import           Foreign.C.Types
import qualified LambdaHeights.Render          as Render
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Scale           as Scale
import           LambdaHeights.Types
import           LambdaHeights.Types.Config
import qualified LambdaHeights.Types.Layer     as Layer
import qualified LambdaHeights.Types.Loop      as Loop
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

createConfig :: ConfigReader RenderConfig
createConfig = do
  loadedFont <- M.asks metaFont
  return $ RenderConfig
    { font              = loadedFont
    , headlineColor     = SDL.V4 255 255 255 255
    , bgColor           = SDL.V4 30 30 30 255
    , playerColor       = SDL.V4 135 31 120 255
    , playerShadowColor = SDL.V4 255 255 255 255
    , textColor         = SDL.V4 0 191 255 255
    }

-- | Renders the state and executes an pre and post action.
render :: (M.MonadIO m) => RenderContext -> RenderConfig -> Loop.Render m State.State
render (window, renderer) config = do
  state      <- Loop.askRenderState
  windowSize <- SDL.get $ SDL.windowSize window
  mapM_ (renderLayer renderer windowSize $ State.screen state) $ State.layers state
  renderPlayer renderer config windowSize (State.screen state) (State.player state)
  renderPlayerShadow renderer config windowSize (State.screen state) (State.player state)
  renderHud renderer config windowSize

-- | Clears the screen with given color.
clear :: SDL.Renderer -> SDLP.Color -> IO ()
clear renderer color = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer

renderHud
  :: (M.MonadIO m) => SDL.Renderer -> RenderConfig -> V2 CInt -> Loop.RenderState m State.State ()
renderHud renderer config windowSize = do
  renderHudVelocity renderer config
  renderHudTime renderer config
  renderHudScore renderer config
  renderHudFPS renderer config windowSize
  renderHudRate renderer config windowSize

renderHudVelocity
  :: (M.MonadIO m) => SDL.Renderer -> RenderConfig -> Loop.RenderState m State.State ()
renderHudVelocity renderer config = do
  state <- Loop.askRenderState
  let textFont = font config
  let V2 x y   = Player.velocity . State.player $ state
  Render.renderText renderer textFont (headlineColor config) (V2 20 20) "VELOCITY"
  Render.renderText renderer textFont (textColor config) (V2 100 20) $ show (round x :: Int)
  Render.renderText renderer textFont (textColor config) (V2 150 20) $ show (round y :: Int)

renderHudTime :: (M.MonadIO m) => SDL.Renderer -> RenderConfig -> Loop.RenderState m State.State ()
renderHudTime renderer config = do
  state <- Loop.askRenderState
  let textFont = font config
  let duration = State.duration state
  let seconds = truncate (realToFrac duration / 1000 :: Float) :: Integer
  let millis   = mod duration 1000
  Render.renderText renderer textFont (headlineColor config) (V2 20 40) "TIME"
  Render.renderText renderer textFont (textColor config) (V2 100 40) $ show seconds
  Render.renderText renderer textFont (textColor config) (V2 150 40) $ show millis

renderHudScore :: (M.MonadIO m) => SDL.Renderer -> RenderConfig -> Loop.RenderState m State.State ()
renderHudScore renderer config = do
  state <- Loop.askRenderState
  let textFont = font config
  let score    = Player.score . State.player $ state
  Render.renderText renderer textFont (headlineColor config) (V2 20 60) "SCORE"
  Render.renderText renderer textFont (textColor config) (V2 100 60) $ show score

renderHudFPS
  :: (M.MonadIO m) => SDL.Renderer -> RenderConfig -> V2 CInt -> Loop.RenderState m State.State ()
renderHudFPS renderer config (V2 w _) = do
  timer <- Loop.askRenderTimer
  let textFont = font config
  let fps      = Timer.fps . Timer.counter $ timer
  let x        = w - 125
  Render.renderText renderer textFont (headlineColor config) (V2 x 20) "FPS"
  Render.renderText renderer textFont (textColor config) (V2 (x + 70) 20) $ show fps

renderHudRate
  :: (M.MonadIO m) => SDL.Renderer -> RenderConfig -> V2 CInt -> Loop.RenderState m State.State ()
renderHudRate renderer config (V2 w _) = do
  timer <- Loop.askRenderTimer
  let textFont = font config
  let rate     = Timer.rate timer
  let x        = w - 125
  Render.renderText renderer textFont (headlineColor config) (V2 x 40) "RATE"
  Render.renderText renderer textFont (textColor config) (V2 (x + 70) 40) $ show rate

data Shape = Shape [Float] [Float]

shapeSize :: Shape -> Size
shapeSize (Shape xs ys) = V2 (maximum xs) (maximum ys)

renderPlayer
  :: (M.MonadIO m)
  => SDL.Renderer
  -> RenderConfig
  -> V2 CInt
  -> Screen.Screen
  -> Player.Player
  -> m ()
renderPlayer renderer config windowSize screen player = do
  let shape = Shape [0, 10, 40, 30, 20, 10, 0, 15] [80, 80, 0, 0, 25, 0, 0, 40]
  let translatedShape =
        centerBottom (Player.position player) $ flipByVel (Player.velocity player) shape
  renderShape renderer windowSize screen (playerColor config) translatedShape

renderPlayerShadow
  :: (M.MonadIO m)
  => SDL.Renderer
  -> RenderConfig
  -> V2 CInt
  -> Screen.Screen
  -> Player.Player
  -> m ()
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

renderShape
  :: (M.MonadIO m) => SDL.Renderer -> V2 CInt -> Screen.Screen -> SDLP.Color -> Shape -> m ()
renderShape renderer (V2 w h) screen color (Shape xs ys) = do
  let toVector   = foldl V.snoc V.empty
  let transformX = fromIntegral . Scale.translate screen w
  let transformY = fromIntegral . Scale.translateFlipped screen h
  let xs'        = toVector . map transformX $ xs
  let ys'        = toVector . map transformY $ ys
  SDLP.fillPolygon renderer xs' ys' color

renderLayer :: (M.MonadIO m) => SDL.Renderer -> V2 CInt -> Screen.Screen -> Layer.Layer -> m ()
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
