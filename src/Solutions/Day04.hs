{-# LANGUAGE TupleSections #-}
module Solutions.Day04 where

import Data.Function (on)
import Data.List (find, partition, transpose)
import Data.List.Split (splitOn, splitWhen)

import Solutions.Util

type Board = [[(Int, Bool)]]

parse :: [String] -> ([Int], [Board])
parse (nl : _ : bls) =
  let
    ns = read <$> splitOn "," nl
    bs = splitWhen null bls
    parseln ln = (, False) . read <$> words ln
  in (ns, fmap (fmap parseln) bs)

win :: Board -> Bool
win b = on (||) (any $ all snd) b (transpose b)

mark :: Int -> Board -> Board
mark n =
  let markSq (i, mk) = if i == n then (i, True) else (i, mk)
  in fmap (fmap markSq)

score :: Int -> Board -> Int
score n b = n * (sum . fmap fst . filter (not . snd) . concat) b

firstScore :: ([Int], [Board]) -> Int
firstScore (n : ns, bs) =
  let bs' = mark n <$> bs
  in
    case find win bs' of
      Nothing -> firstScore (ns, bs')
      Just b  -> score n b

lastScore :: ([Int], [Board]) -> Int
lastScore (n : ns, bs) =
  let (wins, rest) = partition win $ mark n <$> bs
  in if null rest then score n (head wins) else lastScore (ns, rest)

solver :: Solver ([Int], [Board]) Int Int
solver =
  Solver { parseInput = parse . lines, part1 = firstScore, part2 = lastScore }
