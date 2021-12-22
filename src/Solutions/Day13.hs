module Solutions.Day13 where

import Data.Bifunctor (first, second)
import Data.List.Split (chunksOf, splitOn)
import Data.Set (Set)
import qualified Data.Set as S

import Solutions.Util

type Fold = (String, Int)
type Dot = (Int, Int)

-- this is just so we can pretty-print at the end
newtype Paper = Paper (Set Dot)
instance Show Paper where
  show (Paper dots) =
    let
      w = S.findMax $ S.map fst dots
      h = S.findMax $ S.map snd dots
      getChr paper y x = if S.member (x, y) paper then '█' else ' '
      chrs = getChr dots <$> [0 .. h] <*> [0 .. w]
    in unlines . ("\n" :) . chunksOf (w + 1) $ chrs

parse :: String -> (Set Dot, [Fold])
parse input =
  let [dots, folds] = splitOn [""] (lines input)
  in (parseDots dots, parseFolds folds)
 where
  first2 (x : y : _) = (x, y)
  parseDots  = S.fromList . fmap (first2 . fmap read . splitOn ",")
  parseFolds = fmap $ second read . first2 . splitOn "=" . last . words

foldPaper :: Set Dot -> Fold -> Set Dot
foldPaper dots (ax, n) = case ax of
  "x" -> S.map (first moveDot) dots
  "y" -> S.map (second moveDot) dots
  where moveDot d = if d > n then 2 * n - d else d

solver :: Solver (Set Dot, [Fold]) Int Paper
solver = Solver
  { parseInput = parse
  , part1      = S.size . uncurry foldPaper . second head
  , part2      = Paper . uncurry (foldl foldPaper)
  }
