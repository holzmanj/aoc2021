module Solutions.Day01 where

import Data.List (tails)

import Solutions.Util

countInc :: [Int] -> Int
countInc xs = length $ filter (< 0) $ zipWith (-) xs (tail xs)

windowSums :: [Int] -> [Int]
windowSums xs = sum . take 3 <$> tails xs

solver :: Solver [Int] Int Int
solver = Solver
  { parseInput = (read <$>) . lines
  , part1      = countInc
  , part2      = countInc . windowSums
  }
