{-# LANGUAGE OverloadedStrings #-}

module LambdaHeights.ReplayMenu where

import           Data.Either
import           Data.List
import qualified Data.Text                           as T
import           Data.Yaml
import qualified LambdaHeights.Menu                  as Menu
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table                 as Table
import qualified LambdaHeights.Types.ReplayMenuState as ReplayMenu
import qualified LambdaHeights.Types.ReplayState     as Replay
import qualified LambdaHeights.Types.Table           as Table
import qualified LambdaHeights.Types.Timer           as Timer
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                            as SDLF
import           System.Directory

createConfig :: IO Menu.RenderConfig
createConfig = Menu.RenderConfig <$> SDLF.load "fonts/retro_gaming.ttf" 11 <*> SDLF.load
  "fonts/retro_gaming.ttf"
  11

loadReplayFiles :: IO [Replay.Description]
loadReplayFiles = do
  fileNames <- filterPacked (T.isSuffixOf ".desc") <$> listDirectory "replays"
  let filePathes = map ("replays/" ++) fileNames
  sortBy (flip compare) . rights <$> mapM decodeFileEither filePathes

filterPacked :: (T.Text -> Bool) -> [String] -> [String]
filterPacked f = map T.unpack . filter f . map T.pack

buildTable :: [Replay.Description] -> Table.Table
buildTable xs =
  let texts    = tableHeader : ensureRows (map toList xs)
      selected = V2 2 1
  in  Table.newTable texts selected

toList :: Replay.Description -> [String]
toList x =
  let durationSec = realToFrac (Replay.duration x) / 1000 :: Float
  in  [ Replay.fileName x
      , show $ Replay.time x
      , show durationSec
      , show $ Replay.score x
      , show $ Replay.version x
      ]

tableHeader :: [String]
tableHeader = ["file path", "time", "duraction (sec)", "score", "version"]

ensureRows :: [[String]] -> [[String]]
ensureRows [] = [replicate 5 "n/a"]
ensureRows xs = xs

updateSelection :: Table.UpdateTable
updateSelection =
  Table.with Table.toKeycode $ Table.applyKeycode $ Table.limitNotFirstRow $ Table.limitFirstColumn
    Table.limitAll

update
  :: Timer.LoopTimer -> [SDL.Event] -> ReplayMenu.State -> Either (Maybe String) ReplayMenu.State
update _ events state =
  let updated = Menu.update updateSelection id events $ ReplayMenu.table state
  in  case updated of
        Left  result -> Left result
        Right table  -> Right $ updateViewport $ state { ReplayMenu.table = table }

updateViewport :: ReplayMenu.State -> ReplayMenu.State
updateViewport state =
  let viewport = Table.updatePageViewport (ReplayMenu.table state) (ReplayMenu.viewport state)
  in  state { ReplayMenu.viewport = viewport }

render :: RenderContext -> Menu.RenderConfig -> Timer.LoopTimer -> ReplayMenu.State -> IO ()
render (window, renderer) config _ state = do
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
  SDL.clear renderer
  let table    = ReplayMenu.table state
  let viewport = ReplayMenu.viewport state
  let table'   = Table.viewportTable viewport table
  view <- Table.newTableView (Menu.font config) table'
  Menu.render (window, renderer) config view
  SDL.present renderer
