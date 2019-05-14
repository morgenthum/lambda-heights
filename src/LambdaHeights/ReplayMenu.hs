{-# LANGUAGE OverloadedStrings #-}

module LambdaHeights.ReplayMenu where

import           Control.Monad.Extra
import           Data.Matrix
import qualified LambdaHeights.Menu                   as Menu

import qualified Data.Text                            as T
import           Data.Yaml
import qualified LambdaHeights.GUI.Table.CellLocators as Locate
import qualified LambdaHeights.GUI.Table.CellSizers   as Size
import qualified LambdaHeights.GUI.Table.CellStyler   as Style
import qualified LambdaHeights.GUI.Table.TableUpdater as Update
import qualified LambdaHeights.GUI.Table.TextLocators as TextLocate
import qualified LambdaHeights.GUI.Table.Types        as GUI
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.ReplayMenuState  as ReplayMenu
import qualified LambdaHeights.Types.ReplayState      as Replay
import qualified LambdaHeights.Types.Timer            as Timer
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                             as SDLF
import           System.Directory

createConfig :: IO Menu.RenderConfig
createConfig = Menu.RenderConfig <$> SDLF.load "retro_gaming.ttf" 11

newStyler :: SDLF.Font -> GUI.CellStyler GUI.CellStyle
newStyler f =
  let headStyle     = GUI.CellStyle f (V4 0 191 255 255) (V4 30 30 30 255)
      selectedStyle = GUI.CellStyle f (V4 30 30 30 255) (V4 0 191 255 255)
      bodyStyle     = GUI.CellStyle f (V4 30 30 30 255) (V4 255 255 255 255)
  in Style.with $ Style.prefer (Style.header headStyle) $ Style.selectedAndBody selectedStyle bodyStyle

newSizer :: Matrix GUI.Size -> GUI.CellSizer
newSizer fontSizes = Size.alignWidths $ Size.with $ Size.extend (V2 20 20) $ Size.copy fontSizes

newPositioner :: Matrix GUI.Size -> GUI.CellPositioner
newPositioner sm = Locate.with
              $ Locate.indentSelected 10
              $ Locate.addGaps (V2 20 20)
              $ Locate.grid sm

newTextPositioner :: Matrix GUI.Size -> Matrix GUI.Position -> Matrix GUI.Size -> GUI.TextPositioner
newTextPositioner sm pm fontSizes = TextLocate.with $ TextLocate.center sm pm fontSizes

loadReplayFiles :: IO [Replay.Description]
loadReplayFiles = do
  files <- map T.unpack . filter (T.isSuffixOf ".replay.desc") . map T.pack <$> listDirectory "."
  mapMaybeM decodeFile files

buildTable :: [Replay.Description] -> GUI.Table
buildTable xs = GUI.Table (fromLists $ tableHeader : ensureRows (map Replay.toList xs)) (V2 2 1)

tableHeader :: [String]
tableHeader = ["File name", "Time", "Duration (sec)", "Score"]

ensureRows :: [[String]] -> [[String]]
ensureRows [] = [replicate 4 "n/a"]
ensureRows xs = xs

update :: Timer.LoopTimer -> [SDL.Event] -> ReplayMenu.State -> Either String ReplayMenu.State
update timer events state =
  let updater =
        Update.with Update.toSelectEvent
          $ Update.applySelectEvent
          $ Update.limitNotFirstRow
          $ Update.limitFirstColumn Update.limitAll
      updated = Menu.update updater id timer events $ ReplayMenu.table state
  in  case updated of
        Left  result -> Left result
        Right menu   -> Right $ state { ReplayMenu.table = menu }

render :: RenderContext -> Menu.RenderConfig -> Timer.LoopTimer -> ReplayMenu.State -> IO ()
render (window, renderer) config timer state = do
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
  SDL.clear renderer
  let table = ReplayMenu.table state
  view <- defaultView config table
  Menu.render (window, renderer) timer table view
  SDL.present renderer

defaultView :: Menu.RenderConfig -> GUI.Table -> IO (GUI.TableView GUI.CellStyle)
defaultView config table = do
  fontSizes <- Size.loadFontSizes (Menu.font config) $ GUI.content table
  let styles = newStyler (Menu.font config) table
  let sizes = newSizer fontSizes table
  let positions = newPositioner sizes table
  let textPositions = newTextPositioner sizes positions fontSizes table
  return $ GUI.TableView styles sizes positions textPositions