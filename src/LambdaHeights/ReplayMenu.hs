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

createConfig :: GUI.Table -> IO Menu.RenderConfig
createConfig table = do
  f         <- SDLF.load "retro_gaming.ttf" 11
  fontSizes <- Size.loadFontSizes f $ GUI.content table
  let headStyle     = GUI.CellStyle f (V4 0 191 255 255) (V4 30 30 30 255)
  let selectedStyle = GUI.CellStyle f (V4 30 30 30 255) (V4 0 191 255 255)
  let bodyStyle     = GUI.CellStyle f (V4 30 30 30 255) (V4 255 255 255 255)
  return $ Menu.RenderConfig
    { Menu.font       = f
    , Menu.generators = GUI.TableViewGenerators
      { GUI.styleCells = Style.with $ Style.prefer (Style.header headStyle) $ Style.selectedAndBody
        selectedStyle
        bodyStyle
      , GUI.sizeCells = Size.alignWidths $ Size.with $ Size.extend (V2 20 20) $ Size.copy fontSizes
      , GUI.positionCells = Locate.with $ Locate.indentSelected 10 $ Locate.addGaps (V2 20 20)
                                                                                    Locate.grid
      , GUI.positionTexts = TextLocate.with $ TextLocate.center fontSizes
      }
    }

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
  Menu.render (window, renderer) config timer $ ReplayMenu.table state
  SDL.present renderer
