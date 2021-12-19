module Solutions.Day07 where

import Data.List.Split (splitOn)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Solutions.Util

linDist :: Int -> Int -> Int
linDist x y = abs $ x - y

triDist :: Int -> Int -> Int
triDist x y = let d = linDist x y in (d * (d + 1)) `div` 2

solveP1 :: Seq Int -> Int
solveP1 s = sum $ linDist median <$> s
  where median = let mid = S.length s `div` 2 in S.index (S.sort s) mid

solveP2 :: Seq Int -> Int
solveP2 s = minimum $ fuel <$> [minimum s .. maximum s]
  where fuel x = sum $ triDist x <$> s

solver :: Solver (Seq Int) Int Int
solver = Solver
  { parseInput = S.fromList . fmap read . splitOn ","
  , part1      = solveP1
  , part2      = solveP2
  }
