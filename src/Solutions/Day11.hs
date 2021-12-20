module Solutions.Day11 where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Control.Monad.State.Strict
import Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)

import Solutions.Util

type Coord = (Int, Int)
type Energy = Int
type Grid a = Map Coord a

fromLists :: [[Int]] -> Grid Energy
fromLists rs =
  M.fromList [ ((x, y), n) | (y, r) <- zip [0 ..] rs, (x, n) <- zip [0 ..] r ]

neighbors :: Coord -> [Coord]
neighbors (x, y) = [ (i, j) | i <- [x - 1 .. x + 1], j <- [y - 1 .. y + 1] ]

flash :: Grid (Maybe Energy) -> Grid (Maybe Energy)
flash grid = foldl flash1 grid (M.keys grid)
 where
  flash1 g p = if any (> 9) (g ! p)
    then M.insert p Nothing $ foldr (M.adjust $ fmap (+ 1)) g (neighbors p)
    else g

step :: State (Grid Energy) Int
step = do
  grid <- gets (until (not . any (any (> 9))) flash . fmap (Just . (+ 1)))
  put $ fromMaybe 0 <$> grid
  return $ M.size (M.filter isNothing grid)

sync :: State (Grid Energy) [Int]
sync = get >>= (\g -> if all (== 0) g then return [] else liftA2 (:) step sync)

solver :: Solver (Grid Energy) Int Int
solver = Solver
  { parseInput = fromLists . fmap (fmap $ read . pure) . lines
  , part1      = sum . evalState (replicateM 100 step)
  , part2      = length . evalState sync
  }
