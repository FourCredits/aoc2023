module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import Paths_aoc2023 (getDataFileName)
import Text.Printf (printf)

main :: IO ()
main = for_ days $ \(dayNumber, function) -> do
  let file = printf "inputs/day%02d.txt" dayNumber
  input <- getDataFileName file >>= readFileBS <&> decodeUtf8
  let (part1, part2) = function input
  let formatString = "------\nDay %02d\n------\nPart 1: %s\nPart 2: %s\n"
  printf formatString dayNumber part1 part2

type Day = (Int, Text -> (Text, Text))

days :: [Day]
days =
  [ (1, Day01.solve),
    (2, Day02.solve),
    (3, Day03.solve),
    (4, Day04.solve),
    (5, Day05.solve),
    (6, Day06.solve),
    (7, Day07.solve),
    (8, Day08.solve),
    (9, Day09.solve)
  ]
