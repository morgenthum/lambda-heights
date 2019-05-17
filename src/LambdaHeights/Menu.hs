module LambdaHeights.Menu where

import           Data.Matrix
import           Graphics.UI.Table.Combinators
import           Graphics.UI.Table.Render
import           Graphics.UI.Table.Update
import Graphics.UI.Classes
import qualified Graphics.UI.Types as T
import qualified Graphics.UI.Types.Table       as T
import           LambdaHeights.RenderContext
import           LambdaHeights.Types.Timer
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                      as SDLF

type ToResult a = Maybe String -> a

newtype RenderConfig = RenderConfig {
  font :: SDLF.Font
}

createConfig :: IO RenderConfig
createConfig = RenderConfig <$> SDLF.load "HighSchoolUSASans.ttf" 28

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font

keyInput :: IO [SDL.Event]
keyInput = SDL.pollEvents

updateDefault :: ToResult a -> LoopTimer -> [SDL.Event] -> T.Table -> Either a T.Table
updateDefault = update $ with toSelectEvent $ applySelectEvent limitAll

update :: T.UpdateTable -> ToResult a -> LoopTimer -> [SDL.Event] -> T.Table -> Either a T.Table
update updater toResult _ events table =
  let table' = updater events table
  in  if pressedKey SDL.KeycodeReturn events
        then Left $ toResult $ Just $ T.selectedValue table'
        else if pressedKey SDL.KeycodeEscape events then Left $ toResult Nothing else Right table'

pressedKey :: SDL.Keycode -> [SDL.Event] -> Bool
pressedKey code = any (isKeycode code)

isKeycode :: SDL.Keycode -> SDL.Event -> Bool
isKeycode code event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let actualCode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion     = SDL.keyboardEventKeyMotion keyEvent
    in  code == actualCode && motion == SDL.Pressed
  _ -> False

defaultView :: RenderConfig -> T.Table -> IO T.TableView
defaultView config table = do
  fontSizes <- loadFontSizes (font config) $ T.content table
  let styles        = newStyler (font config) table
  let sizes         = newSizer fontSizes table
  let positions     = newPositioner sizes table
  let textPositions = newTextPositioner sizes positions fontSizes table
  return $ T.TableView $ merge table styles sizes positions textPositions

newStyler :: SDLF.Font -> T.CellStyler
newStyler f =
  let selectedStyle = T.CellStyle f (V4 30 30 30 255) (V4 0 191 255 255)
      bodyStyle     = T.CellStyle f (V4 30 30 30 255) (V4 255 255 255 255)
  in  styleWith $ selectedAndBody selectedStyle bodyStyle

newSizer :: Matrix T.Size -> T.CellSizer
newSizer fontSizes = alignWidths $ sizeWith $ extend (V2 20 20) $ copy fontSizes

newPositioner :: Matrix T.Size -> T.CellPositioner
newPositioner sm = positionWith $ indent selectedRow (V2 10 0) $ addGaps (V2 20 20) $ grid sm

newTextPositioner :: Matrix T.Size -> Matrix T.Position -> Matrix T.Size -> T.TextPositioner
newTextPositioner sm pm fontSizes = positionTextWith $ centerText sm pm fontSizes

render :: RenderContext -> LoopTimer -> T.TableView -> IO ()
render (window, renderer) _ view = do
  pos <- calcCenterPosition window $ calcSize view
  renderTable (renderRectCell renderer pos) view
