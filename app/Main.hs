module Main where

import Data.Foldable (for_)
import qualified Day01
import qualified Day02
import qualified Day03
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
  [ (1, Day01.solve),
    (2, Day02.solve),
    (3, Day03.solve)
  ]
