module Utils where

import Paths_aoc2023
import Text.Printf (printf)

realInput :: Int -> IO Text
realInput day = do
  fileName <- getDataFileName (printf "inputs/day%02d.txt" day)
  readFileBS fileName <&> decodeUtf8
