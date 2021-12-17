module Solutions.Day03 where

import Control.Applicative

import Solutions.Util

newtype Bits = Bits [Int]

instance Semigroup Bits where
  (Bits x) <> (Bits y) = Bits $ zipWith (+) x y

instance Monoid Bits where
  mempty = Bits $ repeat 0

bit :: Int -> Bits -> Int
bit i (Bits ns) = ns !! i

fromBitStr :: String -> Bits
fromBitStr = Bits . fmap (\c -> if c == '0' then -1 else 1)

toDec :: Bits -> Int
toDec (Bits bs) = foldl (\x y -> (if y < 0 then 0 else 1) + 2 * x) 0 bs

complement :: Bits -> Bits
complement (Bits bs) = Bits $ negate <$> bs

solveP1 :: [Bits] -> Int
solveP1 ns =
  let
    bSums   = mconcat ns
    gamma   = toDec bSums
    epsilon = toDec $ complement bSums
  in gamma * epsilon

solveP2 :: [Bits] -> Int
solveP2 ns = oxy * co2
 where
  oxy = toDec $ iter 0 (< 0) ns
  co2 = toDec $ iter 0 (> 0) ns

  iter _ _     [n] = n
  iter i check ns  = iter (i + 1) check $ if bit i (mconcat ns) < 0
    then filter (check . bit i) ns
    else filter (not . check . bit i) ns

solver :: Solver [Bits] Int Int
solver = Solver
  { parseInput = fmap fromBitStr . lines
  , part1      = solveP1
  , part2      = solveP2
  }
