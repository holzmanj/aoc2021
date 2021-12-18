{-# LANGUAGE TupleSections #-}
module Solutions.Day05 where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.List (foldl')
import Data.List.Split (splitOn)

import Solutions.Util

type Point = (Int, Int)
type Line = (Point, Point)

type Grid = IntMap Int

gridWidth = 1000

pt2Key :: Point -> M.Key
pt2Key (x, y) = y * gridWidth + x

parsePt :: String -> Point
parsePt s = let (x : y : _) = read <$> splitOn "," s in (x, y)

parseLn :: String -> Line
parseLn s = let [p1, _, p2] = words s in (parsePt p1, parsePt p2)

ptsInLn :: Line -> [Point]
ptsInLn ((x1, y1), (x2, y2))
  | x1 == x2  = zip (repeat x1) (y1 --> y2)
  | y1 == y2  = zip (x1 --> x2) (repeat y1)
  | otherwise = zip (x1 --> x2) (y1 --> y2)
  where i --> j = if i <= j then [i .. j] else [i, (i - 1) .. j]

ortho :: Line -> Bool
ortho ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

numOverlapping :: [Line] -> Int
numOverlapping ls =
  let grid = M.fromListWith (+) $ (, 1) . pt2Key <$> concatMap ptsInLn ls
  in length $ filter (>= 2) $ M.elems grid

solver :: Solver [Line] Int Int
solver = Solver
  { parseInput = fmap parseLn . lines
  , part1      = numOverlapping . filter ortho
  , part2      = numOverlapping
  }
