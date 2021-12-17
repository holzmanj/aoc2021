module Solutions.Day02 where

import Data.Char (isAlpha, isDigit)
import Data.List (foldl')

import Solutions.Util

data Cmd = Forward Int | Down Int | Up Int

parseLn :: String -> Cmd
parseLn s = case words s of
  ("forward" : n : _) -> Forward (read n)
  ("down"    : n : _) -> Down (read n)
  ("up"      : n : _) -> Up (read n)

solveP1 :: [Cmd] -> Int
solveP1 cs = uncurry (*) $ foldl' f (0, 0) cs
 where
  f (pos, dep) cmd = case cmd of
    Forward x -> (pos + x, dep)
    Down    x -> (pos, dep + x)
    Up      x -> (pos, dep - x)

solveP2 :: [Cmd] -> Int
solveP2 cs = let (_, p, d) = foldl' f (0, 0, 0) cs in p * d
 where
  f (aim, pos, dep) cmd = case cmd of
    Forward x -> (aim, pos + x, dep + (aim * x))
    Down    x -> (aim + x, pos, dep)
    Up      x -> (aim - x, pos, dep)

solver :: Solver [Cmd] Int Int
solver = Solver
  { parseInput = (parseLn <$>) . lines
  , part1      = solveP1
  , part2      = solveP2
  }
