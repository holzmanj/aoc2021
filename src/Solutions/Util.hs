{-# LANGUAGE RecordWildCards #-}
module Solutions.Util where

data Solver a b c = Solver
  { parseInput :: String -> a
  , part1      :: a -> b
  , part2      :: a -> c
  }

runSolver :: (Show b, Show c) => Solver a b c -> String -> IO ()
runSolver Solver {..} rawInput = do
  let input = parseInput rawInput
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
