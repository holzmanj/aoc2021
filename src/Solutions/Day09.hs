module Solutions.Day09 where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down(Down))
import Data.Set (Set)
import qualified Data.Set as S

import Solutions.Util

type Coord = (Int, Int)
type HeightMap = Map Coord Int
type Basin = Set Coord

(!) :: HeightMap -> Coord -> Int
(!) = flip (M.findWithDefault 9)

fromLists :: [[Int]] -> HeightMap
fromLists rs =
  let
    (w, h) = (length (head rs), length rs)
    coords = flip (,) <$> [0 .. h - 1] <*> [0 .. w - 1]
  in M.fromList $ zip coords (concat rs)

adjacent :: Coord -> [Coord]
adjacent (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

lowPoints :: HeightMap -> HeightMap
lowPoints m = M.filterWithKey (\k a -> a < minimum ((m !) <$> adjacent k)) m

basinAt :: HeightMap -> Coord -> Basin
basinAt hm lowPt = until (\b -> iter b == b) iter $ S.fromList [lowPt]
 where
  valid b coord = S.notMember coord b && (hm ! coord) < 9
  iter b =
    let pts = filter (valid b) $ concatMap adjacent (S.elems b)
    in foldr S.insert b pts

basins :: HeightMap -> [Basin]
basins hm = basinAt hm <$> M.keys (lowPoints hm)

solver :: Solver HeightMap Int Int
solver = Solver
  { parseInput = fromLists . fmap (fmap $ read . pure) . lines
  , part1      = sum . fmap (+ 1) . lowPoints
  , part2      = product . take 3 . sortOn Down . fmap S.size . basins
  }
