module Solutions.Day12 where

import Data.Bifunctor (second)
import Data.Char (isLower)
import Data.List (nub)
import Data.Map ((!), Map)
import qualified Data.Map as M
import Data.Tuple (swap)

import Solutions.Util

type Node = String
type Graph = Map String [String]

type CanVisit = String -> [String] -> Bool

fromEdges :: [(String, String)] -> Graph
fromEdges es = M.fromListWith (++) $ fwd ++ bwd
 where
  fwd = second pure <$> es
  bwd = second pure . swap <$> es

solve :: CanVisit -> Graph -> Int
solve canGo g = pathsFrom g [] "start"
 where
  pathsFrom _ _ "end" = 1
  pathsFrom g seen cave =
    let seen' = if all isLower cave then cave : seen else seen
    in if canGo cave seen then sum $ pathsFrom g seen' <$> (g ! cave) else 0

oneTwice :: CanVisit
oneTwice cave seen = ((times == 0) || (noneTwice && notStart)) && (times < 2)
 where
  times     = length $ filter (== cave) seen
  noneTwice = length seen == length (nub seen)
  notStart  = cave /= "start"

solver :: Solver Graph Int Int
solver = Solver
  { parseInput = fromEdges . fmap (second (drop 1) . span (/= '-')) . lines
  , part1      = solve notElem
  , part2      = solve oneTwice
  }
