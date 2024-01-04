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
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
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
  [ (01, Day01.solve),
    (02, Day02.solve),
    (03, Day03.solve),
    (04, Day04.solve),
    (05, Day05.solve),
    (06, Day06.solve),
    (07, Day07.solve),
    (08, Day08.solve),
    (09, Day09.solve),
    (10, Day10.solve),
    (11, Day11.solve),
    (12, Day12.solve),
    (13, Day13.solve),
    (14, Day14.solve),
    (15, Day15.solve),
    (16, Day16.solve),
    (17, Day17.solve),
    (18, Day18.solve),
    (19, Day19.solve),
    (20, Day20.solve),
    (21, Day21.solve)
  ]
