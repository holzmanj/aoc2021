module Solutions where

import Solutions.Day01
import Solutions.Util

solveDay :: Int -> String -> IO ()
solveDay 1 = runSolver Solutions.Day01.solver
solveDay x = const $ putStrLn ("No solution for day " ++ show x)
