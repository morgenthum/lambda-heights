{-# LANGUAGE OverloadedStrings #-}

module LambdaHeights.ReplayMenu where

import           Data.Either
import           Data.Matrix
import qualified Data.Text                           as T
import           Data.Yaml
import qualified Graphics.UI.Table.Update            as GUI
import qualified Graphics.UI.Types.Table             as GUI
import qualified LambdaHeights.Menu                  as Menu
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table                 as Table
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
  fileNames <- filterPacked (T.isSuffixOf ".desc") <$> listDirectory "replays"
  let filePathes = map ("replays/" ++) fileNames
  rights <$> mapM decodeFileEither filePathes

filterPacked :: (T.Text -> Bool) -> [String] -> [String]
filterPacked f = map T.unpack . filter f . map T.pack

buildTable :: [Replay.Description] -> GUI.Table
buildTable xs =
  let content  = fromLists $ tableHeader : ensureRows (map Replay.toList xs)
      selected = V2 2 1
  in  GUI.Table content selected

tableHeader :: [String]
tableHeader = ["File name", "Time", "Duration (sec)", "Score"]

ensureRows :: [[String]] -> [[String]]
ensureRows [] = [replicate 4 "n/a"]
ensureRows xs = xs

update
  :: Timer.LoopTimer -> [SDL.Event] -> ReplayMenu.State -> Either (Maybe String) ReplayMenu.State
update _ events state =
  let updater =
        GUI.with GUI.toSelectEvent
          $ GUI.applySelectEvent
          $ GUI.limitNotFirstRow
          $ GUI.limitFirstColumn GUI.limitAll
      updated = Menu.update updater id events $ ReplayMenu.table state
  in  case updated of
        Left  result -> Left result
        Right table  -> Right $ state { ReplayMenu.table = table }

render :: RenderContext -> Menu.RenderConfig -> Timer.LoopTimer -> ReplayMenu.State -> IO ()
render (window, renderer) config _ state = do
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
  SDL.clear renderer
  view <- Table.newTableView (Menu.font config) $ ReplayMenu.table state
  Menu.render (window, renderer) view
  SDL.present renderer
