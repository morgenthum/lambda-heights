module LambdaHeights.Menu where

import qualified LambdaHeights.GUI.Table.CellLocators  as Locate
import qualified LambdaHeights.GUI.Table.CellRenderer  as GUI
import qualified LambdaHeights.GUI.Table.CellSizers    as Size
import qualified LambdaHeights.GUI.Table.CellStyler    as Style
import qualified LambdaHeights.GUI.Table.TableRenderer as GUI
import qualified LambdaHeights.GUI.Table.TableUpdater  as Update
import qualified LambdaHeights.GUI.Table.TextLocators  as TextLocate
import qualified LambdaHeights.GUI.Table.Types         as GUI
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.Timer             as Timer
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                              as SDLF

type ToResult a = String -> a

data RenderConfig = RenderConfig {
  font       :: SDLF.Font,
  generators :: GUI.TableViewGenerators GUI.CellStyle
}

keyInput :: IO [SDL.Event]
keyInput = SDL.pollEvents

update :: ToResult a -> Timer.LoopTimer -> [SDL.Event] -> GUI.Table -> Either a GUI.Table
update toResult _ events table =
  let updater = Update.with Update.toSelectEvent $ Update.applySelectEvent Update.limitAll
      table'  = updater events table
  in  if hasConfirmed events then Left $ toResult $ GUI.selectedValue table' else Right table'

hasConfirmed :: [SDL.Event] -> Bool
hasConfirmed events =
  let eventToKeyEvent event = case SDL.eventPayload event of
        SDL.KeyboardEvent keyEvent ->
          let code   = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
              motion = SDL.keyboardEventKeyMotion keyEvent
          in  keyToKeyEvent code motion
        _ -> False
      keyToKeyEvent SDL.KeycodeReturn SDL.Pressed = True
      keyToKeyEvent _                 _           = False
  in  not . null $ filter id $ map eventToKeyEvent events

render :: RenderContext -> RenderConfig -> Timer.LoopTimer -> GUI.Table -> IO ()
render (window, renderer) config _ table = do
  let view = GUI.generateView (generators config) table
  tablePos <- GUI.calcTablePos window $ GUI.tableSize view
  GUI.renderTable (GUI.renderRectCell renderer tablePos) table view

createConfig :: GUI.Table -> IO RenderConfig
createConfig table = do
  f         <- SDLF.load "HighSchoolUSASans.ttf" 28
  fontSizes <- Size.loadFontSizes f $ GUI.content table
  let selectedStyle = GUI.CellStyle f (V4 30 30 30 255) (V4 0 191 255 255)
  let bodyStyle = GUI.CellStyle f (V4 30 30 30 255) (V4 255 255 255 255)
  return $ RenderConfig
    { font       = f
    , generators = GUI.TableViewGenerators
      { GUI.styleCells = Style.with $ Style.selectedAndBody selectedStyle bodyStyle
      , GUI.sizeCells = Size.alignWidths $ Size.with $ Size.extend (V2 20 20) $ Size.copy fontSizes
      , GUI.positionCells = Locate.with $ Locate.indentSelected 10 $ Locate.addGaps (V2 0 20) Locate.grid
      , GUI.positionTexts = TextLocate.with $ TextLocate.center fontSizes
      }
    }

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font