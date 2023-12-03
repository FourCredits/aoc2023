module Day01 (solve, part1, part2) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find, tails)
import Data.Maybe (mapMaybe)

solve :: String -> (String, String)
solve input =
  let calibrationLines = lines input
      display = maybe "a line had no numbers on it" show
   in (display $ part1 calibrationLines, display $ part2 calibrationLines)

part1, part2 :: [String] -> Maybe Int
part1 = extractCalibrationValue digits
part2 = extractCalibrationValue digitsAndWords

digits, digitsAndWords :: [(String, Int)]
digits =
  [ ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9)
  ]
digitsAndWords =
  digits
    ++ [ ("one", 1),
         ("two", 2),
         ("three", 3),
         ("four", 4),
         ("five", 5),
         ("six", 6),
         ("seven", 7),
         ("eight", 8),
         ("nine", 9)
       ]

extractCalibrationValue :: [(String, Int)] -> [String] -> Maybe Int
extractCalibrationValue numbers =
  fmap sum . traverse (firstAndLast . splitBy numbers)

splitBy :: [(String, Int)] -> String -> [Int]
splitBy numbers input = input & tails & mapMaybe extractFromStart
  where
    extractFromStart substring = numbers & find (matches substring) <&> snd
    matches substring (term, _) = term == take (length term) substring

firstAndLast :: [Int] -> Maybe Int
firstAndLast [] = Nothing
firstAndLast numbers = Just $ head numbers * 10 + last numbers
