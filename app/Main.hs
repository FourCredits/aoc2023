module Main where

import Data.Foldable (for_)
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
import Paths_aoc2023 (getDataFileName)
import Text.Printf (printf)

main :: IO ()
main = for_ days $ \(dayNumber, function) -> do
  let file = printf "inputs/day%02d.txt" dayNumber
  input <- getDataFileName file >>= readFile
  let (part1, part2) = function input
  let formatString = "------\nDay %02d\n------\nPart 1: %s\nPart 2: %s\n"
  printf formatString dayNumber part1 part2

type Day = (Int, String -> (String, String))

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
    (10, Day10.solve)
  ]
