module Main where

import Data.Foldable (for_)
import qualified Day01
import qualified Day02
import Text.Printf (printf)

main :: IO ()
main = for_ days $ \(dayNumber, function) -> do
  let file = printf "day%02d.txt" dayNumber
  input <- readFile file
  let (part1, part2) = function input
  let formatString = "------\nDay %02d\n------\nPart 1: %s\nPart 2: %s\n"
  printf formatString dayNumber part1 part2

type Day = (Int, String -> (String, String))

days :: [Day]
days =
  [ (1, Day01.solve),
    (2, Day02.solve)
  ]
