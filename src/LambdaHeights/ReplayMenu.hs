{-# LANGUAGE OverloadedStrings #-}

module LambdaHeights.ReplayMenu where

import           Control.Monad.Extra
import           Data.Matrix
import qualified Data.Text                           as T
import           Data.Yaml
import           Graphics.UI.Table.Update
import qualified Graphics.UI.Types.Table             as T
import qualified LambdaHeights.Menu                  as Menu
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table                 as T
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
  let descFiles = map T.unpack . filter (T.isSuffixOf ".desc") . map T.pack
  files <- map ("replays/" ++) . descFiles <$> listDirectory "replays"
  mapMaybeM decodeFile files

buildTable :: [Replay.Description] -> T.Table
buildTable xs =
  let content  = fromLists $ tableHeader : ensureRows (map Replay.toList xs)
      selected = V2 2 1
  in  T.Table content selected

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
  view <- T.newTableView (Menu.font config) $ ReplayMenu.table state
  Menu.render (window, renderer) timer view
  SDL.present renderer
