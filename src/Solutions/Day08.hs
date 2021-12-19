module Solutions.Day08 where

import Data.List ((\\), foldl', intersect, permutations)
import Data.List.Split (splitOn)
import Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as M

import Solutions.Util

type Display = ([String], [String])

data Segment = A | B | C | D | E | F | G
  deriving (Enum, Eq, Ord)

type Wire = [Segment]
type Wiring = Map Char Wire

correct :: Map Int [Segment]
correct = M.fromList
  [ (0, [A, B, C, E, F, G])
  , (1, [C, F])
  , (2, [A, C, D, E, G])
  , (3, [A, C, D, F, G])
  , (4, [B, C, D, F])
  , (5, [A, B, D, F, G])
  , (6, [A, B, D, E, F, G])
  , (7, [A, C, F])
  , (8, [A, B, C, D, E, F, G])
  , (9, [A, B, C, D, F, G])
  ]

parseLn :: String -> Display
parseLn s = let [x, y] = splitOn "|" s in (words x, words y)

solveP1 :: [Display] -> Int
solveP1 = length . filter (`elem` [2, 3, 4, 7]) . concatMap (fmap length . snd)

mustBe :: String -> Int -> Wiring -> Wiring
mustBe s i w =
  let
    -- restrict signal char's possible correct segments
    w'   = foldl' (\w' c -> M.insertWith intersect c (correct ! i) w') w s
    -- eliminate correct segments from all other signal letters
    rest = ['a' .. 'g'] \\ s
  in foldl' (\w' c -> M.insertWith (flip (\\)) c (correct ! i) w') w' rest

canBe :: String -> Int -> Wiring -> Bool
canBe s i w
  | length s /= length segs = False
  | otherwise               = any f (permutations segs)
 where
  segs = correct ! i
  f segs = and $ zipWith elem segs ((w !) <$> s)

deduce :: [String] -> Wiring -> Wiring
deduce signals ws = foldl' iter ws signals
 where
  iter w signal = case length signal of
    2 -> (signal `mustBe` 1) w
    3 -> (signal `mustBe` 7) w
    4 -> (signal `mustBe` 4) w
    7 -> (signal `mustBe` 8) w
    _ -> w

solveDisplay :: Display -> Int
solveDisplay (signals, output) = read $ concatMap show digits
 where
  wiring = deduce signals $ M.fromList $ zip ['a' .. 'g'] (repeat [A .. G])
  digits = [ i | sig <- output, i <- [0 .. 9], (sig `canBe` i) wiring ]

solver :: Solver [Display] Int Int
solver = Solver
  { parseInput = fmap parseLn . lines
  , part1      = solveP1
  , part2      = sum . fmap solveDisplay
  }
