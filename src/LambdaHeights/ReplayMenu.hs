{-# LANGUAGE OverloadedStrings #-}

module LambdaHeights.ReplayMenu where

import           Control.Monad.Extra
import           Data.Matrix
import qualified LambdaHeights.Menu                  as Menu

import qualified Data.Text                           as T
import           Data.Yaml
import           Graphics.UI.Table.Combinators
import           Graphics.UI.Table.Update
import qualified Graphics.UI.Types                   as T
import qualified Graphics.UI.Types.Table             as T
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.ReplayMenuState as ReplayMenu
import qualified LambdaHeights.Types.ReplayState     as Replay
import qualified LambdaHeights.Types.Timer           as Timer
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                            as SDLF
import           System.Directory

createConfig :: IO Menu.RenderConfig
createConfig = Menu.RenderConfig <$> SDLF.load "retro_gaming.ttf" 11

loadReplayFiles :: IO [Replay.Description]
loadReplayFiles = do
  files <- map T.unpack . filter (T.isSuffixOf ".replay.desc") . map T.pack <$> listDirectory "."
  mapMaybeM decodeFile files

buildTable :: [Replay.Description] -> T.Table
buildTable xs =
  let content  = fromLists $ tableHeader : ensureRows (map Replay.toList xs)
      selected = V2 2 1
      limit    = Just $ T.Limit (V2 1 1) (V2 9 4)
  in  T.Table content selected limit

tableHeader :: [String]
tableHeader = ["File name", "Time", "Duration (sec)", "Score"]

ensureRows :: [[String]] -> [[String]]
ensureRows [] = [replicate 4 "n/a"]
ensureRows xs = xs

update
  :: Timer.LoopTimer -> [SDL.Event] -> ReplayMenu.State -> Either (Maybe String) ReplayMenu.State
update timer events state =
  let updater =
        with toSelectEvent $ applySelectEvent $ limitNotFirstRow $ limitFirstColumn limitAll
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
  Menu.render (window, renderer) timer view
  SDL.present renderer

defaultView :: Menu.RenderConfig -> T.Table -> IO T.TableView
defaultView config table = do
  let table' = T.resolveLimit table
  fontSizes <- loadFontSizes (Menu.font config) $ T.content table'
  let styles        = newStyler (Menu.font config) table'
  let sizes         = newSizer fontSizes table'
  let positions     = newPositioner sizes table'
  let textPositions = newTextPositioner sizes positions fontSizes table'
  return $ merge table' styles sizes positions textPositions

newStyler :: SDLF.Font -> T.CellStyler
newStyler f =
  let headStyle     = T.CellStyle f (V4 0 191 255 255) (V4 30 30 30 255)
      selectedStyle = T.CellStyle f (V4 30 30 30 255) (V4 0 191 255 255)
      bodyStyle     = T.CellStyle f (V4 30 30 30 255) (V4 255 255 255 255)
  in  styleWith $ prefer (header headStyle) $ selectedAndBody selectedStyle bodyStyle

newSizer :: Matrix T.Size -> T.CellSizer
newSizer fontSizes = alignWidths $ sizeWith $ extend (V2 20 20) $ copy fontSizes

newPositioner :: Matrix T.Size -> T.CellPositioner
newPositioner sm = positionWith $ indent selectedRow (V2 10 0) $ addGaps (V2 20 20) $ grid sm

newTextPositioner :: Matrix T.Size -> Matrix T.Position -> Matrix T.Size -> T.TextPositioner
newTextPositioner sm pm fontSizes = positionTextWith $ centerText sm pm fontSizes
