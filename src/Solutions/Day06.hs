module Solutions.Day06 where

import Data.IntMap.Strict ((!), IntMap)
import qualified Data.IntMap.Strict as M
import Data.List (foldl')
import Data.List.Split (splitOn)

import Solutions.Util

type Counts = IntMap Int

countFish :: [Int] -> Counts
countFish ns =
  let empty = M.fromList $ zip [0 .. 8] (repeat 0)
  in foldl' (\c i -> M.insertWith (+) i 1 c) empty ns

stepDay :: Counts -> Counts
stepDay c =
  let
    (zs : rest) = M.elems c
    shifted     = M.fromList $ zip [0 .. 8] (rest ++ [0])
    duplicated  = M.fromList $ zip [6, 8] (repeat zs)
  in M.unionWith (+) shifted duplicated

totalOnDay :: Int -> Counts -> Int
totalOnDay day = sum . M.elems . (!! day) . iterate stepDay

solver :: Solver Counts Int Int
solver = Solver
  { parseInput = countFish . fmap read . splitOn ","
  , part1      = totalOnDay 80
  , part2      = totalOnDay 256
  }
