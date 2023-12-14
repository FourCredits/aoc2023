module Day01 (solve, part1, part2) where

import qualified Data.Text as T

solve :: Text -> (Text, Text)
solve input =
  let calibrationLines = lines input
      display :: Maybe Int -> Text
      display = maybe "a line had no numbers on it" show
   in (display $ part1 calibrationLines, display $ part2 calibrationLines)

part1, part2 :: [Text] -> Maybe Int
part1 = extractCalibrationValue digits
part2 = extractCalibrationValue digitsAndWords

digits, digitsAndWords :: [(Text, Int)]
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

extractCalibrationValue :: [(Text, Int)] -> [Text] -> Maybe Int
extractCalibrationValue numbers =
  fmap sum . traverse (viaNonEmpty firstAndLast . splitBy numbers)

splitBy :: [(Text, Int)] -> Text -> [Int]
splitBy numbers input = input & T.tails & mapMaybe extractFromStart
  where
    extractFromStart substring = numbers & find (matches substring) <&> snd
    matches substring (term, _) = term == T.take (T.length term) substring

firstAndLast :: NonEmpty Int -> Int
firstAndLast numbers = head numbers * 10 + last numbers
