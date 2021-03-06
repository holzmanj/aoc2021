module Solutions where

import Solutions.Day01
import Solutions.Day02
import Solutions.Day03
import Solutions.Day04
import Solutions.Day05
import Solutions.Day06
import Solutions.Day07
import Solutions.Day08
import Solutions.Day09
import Solutions.Day10
import Solutions.Day11
import Solutions.Day12
import Solutions.Day13
import Solutions.Util

solveDay :: Int -> String -> IO ()
solveDay 1  = runSolver Solutions.Day01.solver
solveDay 2  = runSolver Solutions.Day02.solver
solveDay 3  = runSolver Solutions.Day03.solver
solveDay 4  = runSolver Solutions.Day04.solver
solveDay 5  = runSolver Solutions.Day05.solver
solveDay 6  = runSolver Solutions.Day06.solver
solveDay 7  = runSolver Solutions.Day07.solver
solveDay 8  = runSolver Solutions.Day08.solver
solveDay 9  = runSolver Solutions.Day09.solver
solveDay 10 = runSolver Solutions.Day10.solver
solveDay 11 = runSolver Solutions.Day11.solver
solveDay 12 = runSolver Solutions.Day12.solver
solveDay 13 = runSolver Solutions.Day13.solver
solveDay x  = const $ putStrLn ("No solution for day " ++ show x)
