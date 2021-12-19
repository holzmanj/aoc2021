module Solutions.Day10 where

import Data.Either (lefts, rights)
import Data.List (sort)

import Solutions.Util

type ErrorScore = Int
type CompleteScore = Int

syntaxError :: Char -> Either ErrorScore a
syntaxError = Left . points
 where
  points ')' = 3
  points ']' = 57
  points '}' = 1197
  points '>' = 25137

completion :: String -> Either a CompleteScore
completion = Right . foldl (\acc c -> acc * 5 + points c) 0
 where
  points ')' = 1
  points ']' = 2
  points '}' = 3
  points '>' = 4

match :: Char -> Char
match '(' = ')'
match '[' = ']'
match '{' = '}'
match '<' = '>'

checkSyntax :: String -> Either ErrorScore CompleteScore
checkSyntax = iter []
 where
  iter [] (c : cs) = iter [c] cs
  iter os []       = completion $ foldr ((:) . match) "" os
  iter (o : os) (c : cs)
    | c `elem` "([{<" = iter (c : o : os) cs
    | c == match o    = iter os cs
    | otherwise       = syntaxError c

median :: (Ord a) => [a] -> a
median xs = sort xs !! (length xs `div` 2)

solver :: Solver [String] Int Int
solver = Solver
  { parseInput = lines
  , part1      = sum . lefts . fmap checkSyntax
  , part2      = median . rights . fmap checkSyntax
  }
