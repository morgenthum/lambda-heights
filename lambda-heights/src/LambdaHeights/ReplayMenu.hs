{-# LANGUAGE OverloadedStrings #-}

module LambdaHeights.ReplayMenu where

import ComposeEngine.RenderContext
import qualified ComposeEngine.Types.Loop as Loop
import qualified Control.Monad.IO.Class as M
import qualified Control.Monad.Reader as M
import Data.Either
import Data.List
import qualified Data.Text as T
import Data.Yaml
import qualified LambdaHeights.Menu as Menu
import qualified LambdaHeights.Table as Table
import LambdaHeights.Types.Config
import qualified LambdaHeights.Types.ReplayMenuState as ReplayMenu
import qualified LambdaHeights.Types.ReplayState as Replay
import LambdaHeights.Types.Score
import qualified LambdaHeights.Types.Table as Table
import Linear.V2
import qualified SDL
import System.Directory

createConfig :: ConfigReader Menu.RenderConfig
createConfig = Menu.RenderConfig <$> M.asks metaFont <*> M.asks metaFont

loadReplayFiles :: (M.MonadIO m) => m [Replay.Description]
loadReplayFiles = do
  fileNames <- M.liftIO $ filterPacked (T.isSuffixOf ".desc") <$> listDirectory "replays"
  let filePathes = map ("replays/" ++) fileNames
  M.liftIO $ sortBy (flip compare) . rights <$> mapM decodeFileEither filePathes

filterPacked :: (T.Text -> Bool) -> [String] -> [String]
filterPacked f = map T.unpack . filter f . map T.pack

buildTable :: [Replay.Description] -> Table.Table
buildTable xs =
  let texts = tableHeader : ensureRows (map toList xs)
      selected = V2 2 1
   in Table.newTable texts selected

toList :: Replay.Description -> [String]
toList x =
  let durationSec = realToFrac (Replay.duration x) / 1000 :: Float
      Score score = Replay.score x
   in [ Replay.fileName x,
        show $ Replay.time x,
        show durationSec,
        show score,
        show $ Replay.version x
      ]

tableHeader :: [String]
tableHeader = ["file path", "time", "duraction (sec)", "score", "version"]

ensureRows :: [[String]] -> [[String]]
ensureRows [] = [replicate 5 "n/a"]
ensureRows xs = xs

updateSelection :: Table.UpdateTable
updateSelection =
  Table.with Table.convertKeycode
    $ Table.applyKeycode
    $ Table.limitNotFirstRow
    $ Table.limitFirstColumn Table.limitAll

update :: Loop.Update ReplayMenu.State (Maybe String) [SDL.Event]
update events = do
  state <- Loop.getUpdateState
  case Menu.update updateSelection id events $ ReplayMenu.table state of
    Left result -> Loop.putUpdateResult result
    Right table -> Loop.putUpdateState $ updateViewport $ state {ReplayMenu.table = table}

updateViewport :: ReplayMenu.State -> ReplayMenu.State
updateViewport state =
  let viewport = Table.updatePageViewport (ReplayMenu.table state) (ReplayMenu.viewport state)
   in state {ReplayMenu.viewport = viewport}

render :: (M.MonadIO m) => RenderContext -> Menu.RenderConfig -> Loop.Render m ReplayMenu.State
render ctx config = do
  state <- Loop.askRenderState
  let table = ReplayMenu.table state
  let viewport = ReplayMenu.viewport state
  let table' = Table.viewportTable viewport table
  view <- Table.newTableView (Menu.font config) table'
  Menu.render ctx config view
