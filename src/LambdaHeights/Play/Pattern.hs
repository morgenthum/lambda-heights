module LambdaHeights.Play.Pattern where

import           LambdaHeights.Types
import           Linear.V2

data PatternEntry = PatternEntry {
  entryId       :: Int,
  entrySize     :: Size,
  entryPosition :: Position
}

repeatPattern :: Int -> Int -> [PatternEntry] -> [PatternEntry]
repeatPattern n begin = increasingIdsFrom begin . concat . replicate n

increasingIdsFrom :: Int -> [PatternEntry] -> [PatternEntry]
increasingIdsFrom _ []       = []
increasingIdsFrom i (p : ps) = p { entryId = i } : increasingIdsFrom (i + 1) ps

combine :: Int -> [Int -> [PatternEntry]] -> [PatternEntry]
combine _ [] = []
combine begin (f : fs) =
  let ps  = f begin
      end = begin + length ps
  in  ps ++ combine end fs

leftRightPattern :: Int -> [PatternEntry]
leftRightPattern begin = repeatPattern
  10
  begin
  [ PatternEntry 0 (V2 400 50) (V2 0 150)
  , PatternEntry 0 (V2 500 50) (V2 500 150)
  , PatternEntry 0 (V2 500 50) (V2 0 150)
  , PatternEntry 0 (V2 400 50) (V2 600 150)
  ]

boostPattern :: Int -> [PatternEntry]
boostPattern begin = repeatPattern
  1
  begin
  [ PatternEntry 0 (V2 1000 50) (V2 0 500)
  , PatternEntry 0 (V2 600 300) (V2 200 550)
  , PatternEntry 0 (V2 1000 50) (V2 0 1000)
  ]

stairsPattern :: Int -> [PatternEntry]
stairsPattern begin = repeatPattern
  4
  begin
  [ PatternEntry 0 (V2 300 50) (V2 0 150)
  , PatternEntry 0 (V2 500 50) (V2 500 0)
  , PatternEntry 0 (V2 400 50) (V2 250 150)
  , PatternEntry 0 (V2 400 50) (V2 500 150)
  , PatternEntry 0 (V2 300 50) (V2 600 150)
  , PatternEntry 0 (V2 500 50) (V2 0 0)
  , PatternEntry 0 (V2 400 50) (V2 500 150)
  , PatternEntry 0 (V2 400 50) (V2 250 150)
  ]

highPattern :: Int -> [PatternEntry]
highPattern begin = repeatPattern
  1
  begin
  [ PatternEntry 0 (V2 300 50) (V2 100 500)
  , PatternEntry 0 (V2 300 50) (V2 600 0)
  , PatternEntry 0 (V2 300 50) (V2 200 750)
  , PatternEntry 0 (V2 300 50) (V2 500 750)
  , PatternEntry 0 (V2 300 50) (V2 200 750)
  , PatternEntry 0 (V2 300 50) (V2 500 750)
  , PatternEntry 0 (V2 300 50) (V2 200 750)
  , PatternEntry 0 (V2 300 50) (V2 500 750)
  , PatternEntry 0 (V2 300 50) (V2 200 750)
  , PatternEntry 0 (V2 300 50) (V2 500 750)
  ]
